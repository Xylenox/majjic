;;; majjic.el --- Minimal Jujutsu log UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andy Phan
;; Package-Requires: ((emacs "28.1") (magit-section "4.5.0"))
;; Keywords: tools, vc
;; Version: 0.1.0

;;; Commentary:

;; A small read-only Jujutsu log browser built on top of `magit-section'.
;;
;; V1 intentionally reuses `jj log' for graph rendering and focuses on a
;; navigable buffer where each revision is shown as two visible lines, with a
;; collapsible list of changed files underneath.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'uniquify)
(require 'majjic-model)

(defgroup majjic nil
  "Minimal UI for Jujutsu."
  :group 'tools
  :prefix "majjic-")

(defcustom majjic-program "jj"
  "Executable used to run Jujutsu commands."
  :type 'string
  :group 'majjic)

(defcustom majjic-log-revset nil
  "Revset passed to `jj log'.
When nil, use Jujutsu's default log revset."
  :type '(choice (const :tag "Default" nil)
                 string)
  :group 'majjic)

(defcustom majjic-log-limit nil
  "Maximum number of revisions to show in `majjic-log'.
When nil, do not pass a limit to `jj log'."
  :type '(choice (const :tag "No limit" nil)
                 integer)
  :group 'majjic)

(defcustom majjic-section-loading-delay 0.12
  "Seconds to wait before showing a lazy section loading placeholder."
  :type 'number
  :group 'majjic)

(defcustom majjic-file-summary-limit 2000
  "Maximum number of changed-file summary rows to render inline.
When nil, render every row from `jj diff --summary'."
  :type '(choice (const :tag "No limit" nil)
                 natnum)
  :group 'majjic)

(defcustom majjic-custom-commands nil
  "Custom Majjic commands available from `majjic-run-custom-command'.
Each entry is a plist with these keys:

:name is the unique display name used for completion.
:command is the argv template passed to `majjic-program'.
:preview is an optional argv template to run before `:command'.
:confirm is an optional prompt shown before running `:command'.
:status is an optional status message shown while `:command' runs.
:refresh controls whether Majjic refreshes after success, defaulting to t.

Templates are lists of strings and placeholders.  The `:revset' placeholder
expands to one argument containing the selected revisions as a revset.
The `(:revisions PREFIX)' placeholder expands to repeated PREFIX/revision
argument pairs."
  :type '(repeat sexp)
  :group 'majjic)

(defconst majjic--record-separator "\x1f"
  "Separator used to mark revision records in `jj log' output.")

(defvar-local majjic--repo-root nil
  "Repository root for the current `majjic-log-mode' buffer.")

(defvar-local majjic--mutation-in-progress nil
  "Non-nil while a mutating Jujutsu command is running in this buffer.")

(defvar-local majjic--mutation-process nil
  "The current asynchronous mutating Jujutsu process, if any.")

(defvar-local majjic--refresh-process nil
  "The current asynchronous refresh process, if any.")

(defvar-local majjic--preview-in-progress nil
  "Non-nil while an asynchronous preview command is running in this buffer.")

(defvar-local majjic--preview-process nil
  "The current asynchronous preview process, if any.")

(defvar-local majjic--process-generation 0
  "Generation counter used to ignore stale asynchronous callbacks.")

(defvar-local majjic--render-generation 0
  "Generation counter used to ignore stale asynchronous section reads.")

(defvar-local majjic--marked-change-ids nil
  "List of change ids marked in the current log buffer.")

(defvar-local majjic--mark-overlays nil
  "Overlays used to display persistent revision marks.")

(defvar-local majjic--abandon-selected-commit-ids nil
  "List of commit ids selected in abandon mode.")

(defvar-local majjic--abandon-overlays nil
  "Overlays used to style revisions selected in abandon mode.")

(defvar-local majjic--selected-hunk-heading-overlays nil
  "Overlays used to emphasize hunk headers in the selected file section.")

(defvar-local majjic--rebase-state nil
  "Transient state for `majjic-rebase-mode'.")

(defvar-local majjic--rebase-overlays nil
  "Overlays used to preview rebase source and target in `majjic-rebase-mode'.")

(defvar-local majjic--squash-state nil
  "Transient state for `majjic-squash-mode'.")

(defvar-local majjic--squash-overlays nil
  "Overlays used to preview squash source and destination.")

(defconst majjic--op-preview-max-height 10
  "Maximum height for the temporary undo/redo operation preview window.")

(require 'majjic-jj)
(require 'majjic-render)
(require 'majjic-actions)

(declare-function majjic--clear-rebase-overlays "majjic-render")
(declare-function majjic--change-id-for-commit-id "majjic-jj")
(declare-function majjic--call-jj-capture "majjic-jj")
(declare-function majjic--call-jj-capture-async "majjic-jj")
(declare-function majjic--commit-id-for-change-id "majjic-jj")
(declare-function majjic--describe-args "majjic-jj")
(declare-function majjic--effective-redo-operation-preview "majjic-jj")
(declare-function majjic--effective-undo-operation-preview "majjic-jj")
(declare-function majjic--git-push-change-args "majjic-jj")
(declare-function majjic--git-push-preview-result-async "majjic-jj")
(declare-function majjic--git-push-revision-args "majjic-jj")
(declare-function majjic--jj-error-message "majjic-jj")
(declare-function majjic--log-args "majjic-jj")
(declare-function majjic--operation-process-live-p "majjic-jj")
(declare-function majjic--op-log-preview "majjic-jj")
(declare-function majjic--redo-args "majjic-jj")
(declare-function majjic--rebase-moved-change-ids "majjic-jj")
(declare-function majjic--rebase-target-mode-label "majjic-render")
(declare-function majjic--revision-description "majjic-jj")
(declare-function majjic--reset-section-load "majjic-render")
(declare-function majjic--section-load-process-live-p "majjic-render")
(declare-function majjic--single-parent-commit-id "majjic-jj")
(declare-function majjic--sync-rebase-overlays "majjic-render")
(declare-function majjic--clear-squash-overlays "majjic-render")
(declare-function majjic--squash-args "majjic-jj")
(declare-function majjic--sync-squash-overlays "majjic-render")
(declare-function majjic--undo-args "majjic-jj")
(declare-function magit-section-update-highlight "magit-section" (&optional force))

(defface majjic-abandon-included-row
  '((t :inherit shadow :strike-through t))
  "Face for revisions included in abandon mode."
  :group 'majjic)

(defface majjic-marked-row
  '((t :inherit (error bold)))
  "Face for persistent marks in the main Majjic log."
  :group 'majjic)

(defclass majjic-revision-section (magit-section)
  ((keymap :initform 'majjic-revision-section-map)
   (change-id :initarg :change-id)))

(defclass majjic-summary-section (magit-section)
  ((keymap :initform 'majjic-summary-section-map)
   (render-generation :initarg :render-generation :initform 0)
   (load-token :initform 0)
   (load-process :initform nil)
   (load-timer :initform nil)
   (expanded-file-keys :initarg :expanded-file-keys :initform nil)))

(defclass majjic-file-section (magit-section)
  ((keymap :initform 'majjic-file-section-map)
   (commit-id :initarg :commit-id)
   (path :initarg :path)
   (render-generation :initarg :render-generation :initform 0)
   (load-token :initform 0)
   (load-process :initform nil)
   (load-timer :initform nil)))

(defclass majjic-rename-section (magit-section)
  ((keymap :initform 'majjic-summary-section-map)
   (commit-id :initarg :commit-id)))

(defclass majjic-hunk-section (magit-section)
  ((keymap :initform 'majjic-hunk-section-map)
   (old-start :initarg :old-start)
   (old-count :initarg :old-count)
   (new-start :initarg :new-start)
   (new-count :initarg :new-count)
   (body-prefix :initarg :body-prefix)
   (body-lines :initarg :body-lines)))

(defclass majjic-root-section (magit-section) ())

(defclass majjic-elided-section (magit-section)
  ((keymap :initform 'majjic-summary-section-map)))

(defclass majjic-graph-line-section (magit-section)
  ((keymap :initform 'majjic-summary-section-map)))

(defvar-keymap majjic-git-fetch-map
  :doc "Subcommands for `jj git fetch' in `majjic-log-mode'."
  "f" #'majjic-git-fetch
  "t" #'majjic-git-fetch-tracked)

(defvar-keymap majjic-git-push-map
  :doc "Subcommands for `jj git push' in `majjic-log-mode'."
  "c" #'majjic-git-push-change
  "p" #'majjic-git-push)

(defvar-keymap majjic-git-map
  :doc "Prefix map for Git-backed remote operations in `majjic-log-mode'."
  "f" majjic-git-fetch-map
  "p" majjic-git-push-map)

(defvar-keymap majjic-section-map
  :parent magit-section-mode-map
  "TAB" #'majjic-toggle-at-point
  "SPC" #'majjic-space
  "n" #'majjic-section-forward
  "p" #'majjic-section-backward
  "^" #'majjic-section-up
  "N" #'majjic-new
  "e" #'majjic-edit
  "d" #'majjic-describe
  "M" #'majjic-clear-marks
  "O" #'majjic-op-log-browser
  "B" #'majjic-bookmark-browser
  "a" #'majjic-abandon-start
  "r" #'majjic-rebase-start
  "s" #'majjic-squash-or-rebase-source-descendants
  "u" #'majjic-undo
  "U" #'majjic-redo
  "G" majjic-git-map
  ":" #'majjic-run-custom-command
  "<wheel-up>" #'majjic-mwheel-scroll
  "<wheel-down>" #'majjic-mwheel-scroll
  "<mouse-4>" #'majjic-mwheel-scroll
  "<mouse-5>" #'majjic-mwheel-scroll
  "g" #'majjic-log-refresh)

(define-key majjic-section-map [32] #'majjic-space)

(defvar-keymap majjic-rebase-mode-map
  "o" #'majjic-rebase-set-onto
  "a" #'majjic-rebase-set-after
  "b" #'majjic-rebase-set-before
  "r" #'majjic-rebase-set-source-revision
  "s" #'majjic-rebase-set-source-descendants
  "RET" #'majjic-rebase-apply
  "<return>" #'majjic-rebase-apply
  "C-g" #'majjic-rebase-cancel)

(defvar-keymap majjic-squash-mode-map
  "RET" #'majjic-squash-apply
  "<return>" #'majjic-squash-apply
  "C-g" #'majjic-squash-cancel
  "s" #'majjic-squash-disabled-command)

(defvar-keymap majjic-revision-section-map
  :parent majjic-section-map)

(defvar-keymap majjic-summary-section-map
  :parent majjic-section-map)

(defvar-keymap majjic-file-section-map
  :parent majjic-section-map
  "RET" #'majjic-visit-file
  "<return>" #'majjic-visit-file)

(defvar-keymap majjic-hunk-section-map
  :parent majjic-section-map
  "RET" #'majjic-visit-file
  "<return>" #'majjic-visit-file)

(define-derived-mode majjic-log-mode magit-section-mode "JJ-Log"
  "Major mode for browsing `jj log' output."
  :group 'majjic
  (setq-local revert-buffer-function #'majjic--revert-buffer)
  (setq-local truncate-lines t)
  (add-hook 'post-command-hook #'majjic--update-selected-hunk-heading nil t)
  (add-hook 'post-command-hook #'majjic--sync-mark-overlays nil t)
  (add-hook 'post-command-hook #'majjic--sync-rebase-overlays nil t)
  (add-hook 'post-command-hook #'majjic--sync-squash-overlays nil t))

(defvar-keymap majjic-snapshot-mode-map
  "q" #'quit-window)

(define-minor-mode majjic-snapshot-mode
  "Minor mode for read-only `majjic' snapshot buffers.
\\<majjic-snapshot-mode-map>\\[quit-window] closes the temporary snapshot window."
  :group 'majjic
  :keymap majjic-snapshot-mode-map)

(define-minor-mode majjic-rebase-mode
  "Minor mode for staging a `jj rebase' interactively in a `majjic' log buffer."
  :lighter " Rebase"
  :keymap majjic-rebase-mode-map
  (unless (derived-mode-p 'majjic-log-mode)
    (setq majjic-rebase-mode nil)
    (user-error "Not in a majjic log buffer"))
  (unless majjic-rebase-mode
    (setq majjic--rebase-state nil)
    (majjic--clear-rebase-overlays))
  (majjic--sync-rebase-overlays)
  (force-mode-line-update))

(define-minor-mode majjic-squash-mode
  "Minor mode for staging a `jj squash' interactively in a `majjic' log buffer."
  :lighter " Squash"
  :keymap majjic-squash-mode-map
  (unless (derived-mode-p 'majjic-log-mode)
    (setq majjic-squash-mode nil)
    (user-error "Not in a majjic log buffer"))
  (unless majjic-squash-mode
    (setq majjic--squash-state nil)
    (majjic--clear-squash-overlays))
  (majjic--sync-squash-overlays)
  (force-mode-line-update))

(keymap-set majjic-log-mode-map "n" #'majjic-section-forward)
(keymap-set majjic-log-mode-map "p" #'majjic-section-backward)
(keymap-set majjic-log-mode-map "^" #'majjic-section-up)
(keymap-set majjic-log-mode-map "N" #'majjic-new)
(keymap-set majjic-log-mode-map "e" #'majjic-edit)
(keymap-set majjic-log-mode-map "d" #'majjic-describe)
(keymap-set majjic-log-mode-map "M" #'majjic-clear-marks)
(keymap-set majjic-log-mode-map "O" #'majjic-op-log-browser)
(keymap-set majjic-log-mode-map "B" #'majjic-bookmark-browser)
(keymap-set majjic-log-mode-map "a" #'majjic-abandon-start)
(keymap-set majjic-log-mode-map "r" #'majjic-rebase-start)
(keymap-set majjic-log-mode-map "s" #'majjic-squash-or-rebase-source-descendants)
(keymap-set majjic-log-mode-map "u" #'majjic-undo)
(keymap-set majjic-log-mode-map "U" #'majjic-redo)
(keymap-set majjic-log-mode-map "G" majjic-git-map)
(keymap-set majjic-log-mode-map ":" #'majjic-run-custom-command)
(keymap-set majjic-log-mode-map "<wheel-up>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<wheel-down>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<mouse-4>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<mouse-5>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "g" #'majjic-log-refresh)

(defun majjic-space ()
  "Toggle a persistent mark at point.
If point is on a revision heading, move to the next revision afterward.
This is bound on section keymaps so it wins over inherited Magit
section bindings that would otherwise page the buffer."
  (interactive)
  (let* ((section (or (magit-current-section)
                      (user-error "No section selected")))
         (revision (or (majjic--current-revision-section)
                       (user-error "No revision selected")))
         (advancep (object-of-class-p section 'majjic-revision-section)))
    (majjic-toggle-mark)
    (when advancep
      (when-let* ((next (majjic--next-revision-section revision)))
        (magit-section-goto next)))
    (majjic--sync-mark-overlays)))

(defun majjic (&optional directory)
  "Open a Jujutsu log buffer for the current repository.
When DIRECTORY is non-nil, start repository discovery there.
If the current directory is not inside a Jujutsu repository, prompt for one."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Open Jujutsu repository: "
                                default-directory nil t))))
  (let* ((source-dir (file-name-as-directory (expand-file-name (or directory default-directory))))
         (repo-root (or (majjic--locate-root source-dir)
                        (let* ((picked-dir
                                (file-name-as-directory
                                 (expand-file-name
                                  (read-directory-name "Open Jujutsu repository: "
                                                       source-dir nil t))))
                               (picked-root (majjic--locate-root picked-dir)))
                          (unless picked-root
                            (user-error "Not inside a Jujutsu repository: %s"
                                        (abbreviate-file-name picked-dir)))
                          (setq source-dir picked-dir)
                          picked-root)))
         (buffer (majjic--get-log-buffer repo-root)))
    (with-current-buffer buffer
      (majjic-log-mode)
      (setq default-directory source-dir)
      (setq majjic--repo-root repo-root)
      (setq list-buffers-directory (abbreviate-file-name repo-root))
      (majjic--log-refresh-sync (majjic--capture-refresh-state)))
    (pop-to-buffer buffer)))

(defun majjic-log-refresh ()
  "Refresh the current Jujutsu log buffer asynchronously."
  (interactive)
  (unless (derived-mode-p 'majjic-log-mode)
    (user-error "Not in a majjic log buffer"))
  (when majjic--mutation-in-progress
    (user-error "Operation in progress"))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--log-refresh-async (majjic--capture-refresh-state)))

(defun majjic-new ()
  "Create a new child of marked revisions, or current revision, and move to `@'."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let ((parent-ids (or (majjic--marked-visible-commit-ids)
                        (list (majjic--require-current-commit-id)))))
    (majjic--run-mutation
     (lambda ()
       (cons "new" parent-ids))
     :target #'majjic--working-copy-commit-id)))

(defun majjic-edit ()
  "Edit the current revision and move to `@'."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let ((commit-id (majjic--require-current-commit-id)))
    (majjic--run-mutation
     (lambda ()
       (list "edit" "-r" commit-id))
     :target #'majjic--working-copy-commit-id)))

(defun majjic-describe ()
  "Describe the current revision using a minibuffer prompt."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let* ((commit-id (majjic--require-current-commit-id))
         (change-id (majjic--change-id-for-commit-id commit-id))
         (description (majjic--revision-description commit-id))
         (message (read-string "Description: " description)))
    (majjic--status-message "Describing selected revision...")
    (majjic--run-mutation
     (lambda ()
       (majjic--describe-args commit-id message))
     :target (lambda ()
               (or (majjic--commit-id-for-change-id change-id)
                   (majjic--working-copy-commit-id))))))

(defun majjic-undo ()
  "Undo the latest Jujutsu operation after confirmation."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--run-confirmed-op-mutation
   "undo"
   (lambda ()
     (majjic--undo-args))))

(defun majjic-redo ()
  "Redo the latest undone Jujutsu operation after confirmation."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--run-confirmed-op-mutation
   "redo"
   (lambda ()
     (majjic--redo-args))))

(defun majjic--set-operation-status (status)
  "Show STATUS in the mode line for the current Majjic buffer."
  (setq mode-line-process (when status (list (format " %s" status))))
  (force-mode-line-update))

(defun majjic--operation-in-progress-p ()
  "Return non-nil if a mutation, refresh, or preview is in flight."
  (or majjic--mutation-in-progress
      majjic--preview-in-progress
      (majjic--operation-process-live-p majjic--preview-process)
      (majjic--operation-process-live-p majjic--refresh-process)))

(defun majjic--require-no-operation-in-progress ()
  "Signal a user error if an async operation is currently in flight."
  (when (majjic--operation-in-progress-p)
    (user-error "Operation in progress")))

(defun majjic--status-message (message &rest args)
  "Show MESSAGE with ARGS in the minibuffer before an asynchronous operation."
  (apply #'message message args)
  (redisplay))

(defun majjic-run-custom-command (name)
  "Run the custom Majjic command named NAME."
  (interactive (list (majjic--read-custom-command-name)))
  (let ((command (majjic--custom-command-by-name name)))
    (majjic--run-custom-command command)))

(defun majjic--read-custom-command-name ()
  "Read a custom command name from the minibuffer."
  (let ((commands (majjic--validated-custom-commands)))
    (completing-read "Majjic command: "
                     (mapcar (lambda (command) (plist-get command :name))
                             commands)
                     nil t)))

(defun majjic--validated-custom-commands ()
  "Return `majjic-custom-commands' after validating their names."
  (unless majjic-custom-commands
    (user-error "No Majjic custom commands configured"))
  (let ((names nil))
    (dolist (command majjic-custom-commands)
      (let ((name (plist-get command :name)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (user-error "Majjic custom command is missing a string :name"))
        (when (member name names)
          (user-error "Duplicate Majjic custom command name: %s" name))
        (push name names)))
    majjic-custom-commands))

(defun majjic--custom-command-by-name (name)
  "Return the custom command named NAME."
  (or (seq-find (lambda (command)
                  (equal (plist-get command :name) name))
                (majjic--validated-custom-commands))
      (user-error "No Majjic custom command named: %s" name)))

(defun majjic--run-custom-command (command)
  "Run custom COMMAND from `majjic-custom-commands'."
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let* ((name (plist-get command :name))
         (commit-ids (majjic--selected-command-commit-ids))
         (command-args (majjic--custom-command-expanded-args command :command commit-ids))
         (preview-args (majjic--custom-command-expanded-args command :preview commit-ids t))
         (confirm (plist-get command :confirm)))
    (if preview-args
        (majjic--run-custom-command-with-preview command command-args preview-args confirm)
      (when (or (null confirm) (y-or-n-p confirm))
        (majjic--start-custom-command command command-args)))))

(defun majjic--selected-command-commit-ids ()
  "Return visible marked commit ids, or the current commit id."
  (or (majjic--marked-visible-commit-ids)
      (list (majjic--require-current-commit-id))))

(defun majjic--custom-command-expanded-args (command key commit-ids &optional optional)
  "Return expanded argv for COMMAND's KEY using COMMIT-IDS.
When OPTIONAL is non-nil, return nil if KEY is absent."
  (let ((template (plist-get command key)))
    (cond
     ((and optional (null template)) nil)
     ((null template)
      (user-error "Majjic custom command %s is missing %s"
                  (plist-get command :name) key))
     ((not (listp template))
      (user-error "Majjic custom command %s must be a list for %s"
                  (plist-get command :name) key))
     (t
      (majjic--expand-custom-command-template template commit-ids)))))

(defun majjic--expand-custom-command-template (template commit-ids)
  "Expand custom command TEMPLATE using COMMIT-IDS."
  (apply #'append
         (mapcar (lambda (part)
                   (majjic--expand-custom-command-part part commit-ids))
                 template)))

(defun majjic--expand-custom-command-part (part commit-ids)
  "Expand one custom command template PART using COMMIT-IDS."
  (cond
   ((stringp part)
    (list part))
   ((eq part :revset)
    (list (mapconcat #'identity commit-ids " | ")))
   ((and (consp part) (eq (car part) :revisions))
    (let ((prefix (cadr part)))
      (unless (and (stringp prefix) (= (length part) 2))
        (user-error "Invalid :revisions placeholder: %S" part))
      (cl-mapcan (lambda (commit-id)
                   (list prefix commit-id))
                 commit-ids)))
   (t
    (user-error "Invalid custom command template part: %S" part))))

(defun majjic--run-custom-command-with-preview (command command-args preview-args confirm)
  "Preview COMMAND with PREVIEW-ARGS before running COMMAND-ARGS."
  (let ((name (plist-get command :name)))
    (majjic--status-message "Preparing %s preview..." name)
    (setq majjic--preview-in-progress t)
    (majjic--set-operation-status "Previewing")
    (condition-case err
        (let (preview-finished
              process)
          (setq process
                (majjic--custom-command-preview-result-async
                 preview-args
                 (lambda (exit-code preview)
                   (setq preview-finished t)
                   (setq majjic--preview-in-progress nil)
                   (setq majjic--preview-process nil)
                   (majjic--set-operation-status nil)
                   (if (zerop exit-code)
                       (when (or (null confirm)
                                 (majjic--confirm-preview name preview confirm))
                         (majjic--start-custom-command command command-args))
                     (majjic--show-preview name preview
                                           (format "%s preview failed; press any key to close" name))
                     (message "%s preview failed" name)))))
          (unless preview-finished
            (setq majjic--preview-process process)))
      (error
       (setq majjic--preview-in-progress nil)
       (setq majjic--preview-process nil)
       (majjic--set-operation-status nil)
       (signal (car err) (cdr err))))))

(defun majjic--custom-command-preview-result-async (args callback)
  "Run custom preview ARGS and invoke CALLBACK with (EXIT-CODE OUTPUT)."
  (apply #'majjic--call-jj-capture-async majjic--repo-root
         (lambda (exit-code stdout stderr)
           (funcall callback
                    exit-code
                    (string-trim-right
                     (if (string-blank-p stdout) stderr stdout))))
         args))

(defun majjic--start-custom-command (command args)
  "Run custom COMMAND with expanded ARGS."
  (when-let* ((status (plist-get command :status)))
    (majjic--status-message "%s" status))
  (majjic--run-mutation
   (lambda () args)
   :refresh (if (plist-member command :refresh)
                (plist-get command :refresh)
              t)))

(defun majjic-git-fetch (&optional tracked)
  "Fetch from the configured Git remote and refresh.
When TRACKED is non-nil, fetch only tracked bookmarks."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--status-message
   (if tracked
       "Fetching tracked bookmarks from Git remote..."
     "Fetching from Git remote..."))
  (majjic--run-mutation
   (lambda ()
     (majjic--git-fetch-args tracked))))

(defun majjic-git-fetch-tracked ()
  "Fetch only tracked bookmarks from the configured Git remote and refresh."
  (interactive)
  (majjic-git-fetch t))

(defun majjic--git-push-selected (args-function confirm-prompt push-status)
  "Push selected revisions after previewing with ARGS-FUNCTION.
CONFIRM-PROMPT is passed to `majjic--confirm-preview'.  PUSH-STATUS is
shown while the confirmed push runs."
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let ((commit-ids (or (majjic--marked-visible-commit-ids)
                        (list (majjic--require-current-commit-id)))))
    (majjic--status-message "Preparing Git push dry-run...")
    (setq majjic--preview-in-progress t)
    (majjic--set-operation-status "Previewing")
    (condition-case err
        (let (preview-finished
              process)
          (setq process
                (majjic--git-push-preview-result-async
                 commit-ids
                 args-function
                 (lambda (exit-code preview)
                   (setq preview-finished t)
                   (setq majjic--preview-in-progress nil)
                   (setq majjic--preview-process nil)
                   (majjic--set-operation-status nil)
                   (if (zerop exit-code)
                       (when (majjic--confirm-preview "push" preview confirm-prompt)
                         (majjic--status-message push-status)
                         (majjic--run-mutation
                          (lambda ()
                            (funcall args-function commit-ids))))
                     (majjic--show-preview "push" preview "Git push dry-run failed; press any key to close")
                     (message "Git push dry-run failed")))))
          (unless preview-finished
            (setq majjic--preview-process process)))
      (error
       (setq majjic--preview-in-progress nil)
       (setq majjic--preview-process nil)
       (majjic--set-operation-status nil)
       (signal (car err) (cdr err))))))

(defun majjic-git-push-change ()
  "Push marked visible revisions, or the current revision, by change.
Ask for confirmation after a dry-run preview."
  (interactive)
  (majjic--git-push-selected
   #'majjic--git-push-change-args
   "Push selected changes? "
   "Pushing selected changes..."))

(defun majjic-git-push ()
  "Push marked visible revisions, or the current revision, by revision.
Ask for confirmation after a dry-run preview."
  (interactive)
  (majjic--git-push-selected
   #'majjic--git-push-revision-args
   "Push selected revisions? "
   "Pushing selected revisions..."))

(defun majjic-abandon-start ()
  "Abandon marked visible revisions after confirmation.
Temporarily apply strike-through rendering while the confirmation prompt is
open.  If rebase mode is active, keep using `a' for rebase-after instead."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (if majjic-rebase-mode
      (majjic-rebase-set-after)
    (let* ((selected-ids (majjic--marked-visible-commit-ids))
           (count (length selected-ids))
           (fallbacks (majjic--selection-fallbacks (majjic--current-commit-id))))
      (when (zerop count)
        (user-error "No marked revisions to abandon"))
      (unwind-protect
          (progn
            (setq majjic--abandon-selected-commit-ids selected-ids)
            (majjic--clear-mark-overlays)
            (majjic--sync-abandon-overlays)
            (when (y-or-n-p (format "Abandon %d marked revision%s? "
                                    count (if (= count 1) "" "s")))
              (setq majjic--abandon-selected-commit-ids nil)
              (majjic--clear-abandon-overlays)
              (majjic--run-mutation
               (lambda ()
                 (append (list "abandon" "--retain-bookmarks")
                         (majjic--prefixed-rev-args selected-ids)))
               :target (lambda ()
                         (or (seq-find #'majjic--revision-exists-p fallbacks)
                             (majjic--working-copy-commit-id))))))
        (setq majjic--abandon-selected-commit-ids nil)
        (majjic--clear-abandon-overlays)
        (majjic--sync-mark-overlays)))))

(defun majjic-rebase-start ()
  "Enter rebase mode using the current revision as the source.
When already in rebase mode, switch the source back to a single revision."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (if majjic-rebase-mode
      (majjic-rebase-set-source-revision)
    (let* ((visible-marked (majjic--marked-visible-change-ids))
           (source-change-id
            (cond
             ((null visible-marked)
              (majjic--change-id-for-commit-id (majjic--require-current-commit-id)))
             ((= (length visible-marked) 1)
              (car visible-marked))
             (t visible-marked))))
      (setq majjic--rebase-state
            (make-majjic-rebase-state
             :source-change-id source-change-id
             :source-mode 'revision
             :moved-change-ids (majjic--rebase-moved-change-ids source-change-id 'revision)
             :target-mode 'onto))
      (majjic-rebase-mode 1)
      (message "Rebase mode: r revision, s descendants, o onto, a after, b before, RET apply, C-g cancel"))))

(defun majjic-squash-or-rebase-source-descendants ()
  "Enter squash mode, or select descendants while staging a rebase."
  (interactive)
  (if majjic-rebase-mode
      (majjic-rebase-set-source-descendants)
    (majjic-squash-start)))

(defun majjic-squash-start ()
  "Enter squash mode using marked revisions, or the current revision, as source."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let* ((visible-marked (majjic--marked-visible-change-ids))
         (source-change-ids
          (or visible-marked
              (list (majjic--change-id-for-commit-id
                     (majjic--require-current-commit-id))))))
    (setq majjic--squash-state
          (make-majjic-squash-state
           :source-change-ids source-change-ids))
    (majjic-squash-mode 1)
    (when-let* ((destination (majjic--squash-default-destination-commit-id
                              source-change-ids)))
      (majjic--goto-commit destination)
      (majjic--sync-squash-overlays))
    (message "Squash mode: parent selected when available, RET apply, C-g cancel")))

(defun majjic--squash-default-destination-commit-id (source-change-ids)
  "Return the common unique parent commit id for SOURCE-CHANGE-IDS, or nil."
  (let ((parents
         (delq nil
               (mapcar
                (lambda (change-id)
                  (when-let* ((commit-id (majjic--commit-id-for-change-id change-id)))
                    (majjic--single-parent-commit-id commit-id)))
                source-change-ids))))
    (when (and (= (length parents) (length source-change-ids))
               (let ((first (car parents)))
                 (seq-every-p (lambda (parent) (equal parent first)) parents)))
      (car parents))))

(defun majjic-toggle-mark ()
  "Toggle a persistent mark on the current revision."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (let* ((revision (or (majjic--current-revision-section)
                       (user-error "No revision selected")))
         (change-id (oref revision change-id)))
    (unless change-id
      (user-error "No revision selected"))
    (setq majjic--marked-change-ids
          (if (member change-id majjic--marked-change-ids)
              (cl-remove change-id majjic--marked-change-ids :test #'equal)
            (cons change-id majjic--marked-change-ids)))
    (majjic--restyle-marked-revision revision)
    (majjic--refresh-rebase-source-from-marks)
    (message "%s mark"
             (if (member change-id majjic--marked-change-ids) "Added" "Removed"))))

(defun majjic-clear-marks ()
  "Clear all persistent marks in the current log buffer."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (setq majjic--marked-change-ids nil)
  (majjic--clear-mark-overlays)
  (majjic--refresh-rebase-source-from-marks)
  (message "Cleared marks"))

(defun majjic-op-log-browser ()
  "Open the dedicated operation log browser.
This keybinding is reserved; the browser itself is not implemented yet."
  (interactive)
  (user-error "Op-log browser is not implemented yet"))

(defun majjic-bookmark-browser ()
  "Open the dedicated bookmark browser.
This keybinding is reserved; the browser itself is not implemented yet."
  (interactive)
  (user-error "Bookmark browser is not implemented yet"))

(defun majjic-rebase-set-onto ()
  "Set the rebase target placement to onto."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--rebase-set-target-mode 'onto))

(defun majjic-rebase-set-after ()
  "Set the rebase target placement to after."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--rebase-set-target-mode 'after))

(defun majjic-rebase-set-before ()
  "Set the rebase target placement to before."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--rebase-set-target-mode 'before))

(defun majjic-rebase-set-source-revision ()
  "Set the rebase source to the selected revision only."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--rebase-set-source-mode 'revision))

(defun majjic-rebase-set-source-descendants ()
  "Set the rebase source to the selected revision and its descendants."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (majjic--rebase-set-source-mode 'descendants))

(defun majjic-rebase-apply ()
  "Apply the staged rebase."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (when majjic-squash-mode
    (majjic-squash-disabled-command))
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (let* ((source (majjic-rebase-state-source-change-id majjic--rebase-state))
         (sources (majjic--rebase-source-change-ids source))
         (target (majjic--require-current-commit-id))
         (target-change-id (majjic--change-id-for-commit-id target))
         (source-mode (majjic-rebase-state-source-mode majjic--rebase-state))
         (moved-change-ids (majjic-rebase-state-moved-change-ids majjic--rebase-state))
         (target-mode (majjic-rebase-state-target-mode majjic--rebase-state)))
    (when (member target-change-id moved-change-ids)
      (user-error "Rebase target is inside moved revisions"))
    (majjic--run-mutation
     (lambda ()
       (majjic--rebase-args source target target-mode source-mode))
     :after-success (lambda ()
                      (majjic-rebase-mode -1))
     :target (lambda ()
               (or (majjic--commit-id-for-change-id (car sources))
                   (majjic--working-copy-commit-id))))))

(defun majjic-rebase-cancel ()
  "Cancel rebase mode without mutating the repository."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (majjic-rebase-mode -1)
  (message "Rebase canceled"))

(defun majjic-rebase-disabled-command ()
  "Reject commands that are unavailable while staging a rebase."
  (interactive)
  (user-error "Disabled in rebase mode"))

(defun majjic-squash-apply ()
  "Apply the staged squash into the current revision."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (unless majjic-squash-mode
    (user-error "Not in squash mode"))
  (let* ((sources (majjic-squash-state-source-change-ids majjic--squash-state))
         (destination (majjic--require-current-commit-id))
         (destination-change-id (majjic--change-id-for-commit-id destination)))
    (when (member destination-change-id sources)
      (user-error "Squash destination is inside source revisions"))
    (majjic--run-mutation
     (lambda ()
       (majjic--squash-args sources destination))
     :after-success (lambda ()
                      (majjic-squash-mode -1))
     :target (lambda ()
               (let ((fallbacks (majjic--selection-fallbacks destination)))
                 (or (majjic--commit-id-for-change-id destination-change-id)
                     (and (majjic--revision-exists-p destination) destination)
                     (seq-find #'majjic--revision-exists-p fallbacks)
                     (majjic--working-copy-commit-id)))))))

(defun majjic-squash-cancel ()
  "Cancel squash mode without mutating the repository."
  (interactive)
  (majjic--require-no-operation-in-progress)
  (unless majjic-squash-mode
    (user-error "Not in squash mode"))
  (majjic-squash-mode -1)
  (message "Squash canceled"))

(defun majjic-squash-disabled-command ()
  "Reject commands that are unavailable while staging a squash."
  (interactive)
  (user-error "Disabled in squash mode"))

(defun majjic--run-confirmed-op-mutation (action thunk)
  "Preview the latest operation, confirm ACTION, and run mutating THUNK."
  (let* ((current (majjic--current-commit-id))
         (current-change-id (and current (majjic--change-id-for-commit-id current)))
         (fallbacks (majjic--selection-fallbacks current)))
    (when (majjic--confirm-latest-operation action)
      (majjic--run-mutation
       thunk
       :target (lambda ()
                 (or (and current-change-id
                          (majjic--commit-id-for-change-id current-change-id))
                     (and current (majjic--revision-exists-p current) current)
                     (seq-find #'majjic--revision-exists-p fallbacks)
                     (majjic--working-copy-commit-id)))))))

(defun majjic--confirm-latest-operation (action)
  "Show the effective operation preview in a side window and ask to perform ACTION."
  (majjic--confirm-preview
   action
   (pcase action
     ("undo" (majjic--effective-undo-operation-preview))
     ("redo" (majjic--effective-redo-operation-preview))
     (_ (majjic--op-log-preview)))
   (format "%s latest change? " (capitalize action))))

(defun majjic--confirm-preview (action preview prompt)
  "Show PREVIEW for ACTION in a side window and ask PROMPT in the minibuffer."
  (majjic--with-preview-window action preview (lambda () (y-or-n-p prompt))))

(defun majjic--show-preview (action preview prompt)
  "Show PREVIEW for ACTION in a side window and wait for one key with PROMPT."
  (majjic--with-preview-window action preview
                               (lambda ()
                                 (message "%s" prompt)
                                 (redisplay)
                                 (read-key)
                                 nil)))

(defun majjic--with-preview-window (action preview body)
  "Show PREVIEW for ACTION in a side window while running BODY."
  (let ((buffer (get-buffer-create (format "*majjic %s preview*" action)))
        (window nil))
    (unwind-protect
        (save-selected-window
          (majjic--prepare-op-preview-buffer buffer preview)
          (setq window
                (display-buffer-in-side-window
                 buffer
                 `((side . bottom)
                   (slot . 0)
                   (window-height . fit-window-to-buffer)
                   (window-parameters
                    . ((no-other-window . t)
                       (no-delete-other-windows . t))))))
          (when (window-live-p window)
            (set-window-dedicated-p window t)
            (fit-window-to-buffer window majjic--op-preview-max-height 3 nil nil t))
          (funcall body))
      (when (window-live-p window)
        (quit-window t window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun majjic--prepare-op-preview-buffer (buffer preview)
  "Populate BUFFER with PREVIEW text and render ANSI colors."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert preview)
      (unless (bolp)
        (insert "\n"))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))
    (special-mode))
  buffer)

(defun majjic--rebase-set-target-mode (target-mode)
  "Set TARGET-MODE in the current rebase state."
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (setf (majjic-rebase-state-target-mode majjic--rebase-state) target-mode)
  (majjic--sync-rebase-overlays)
  (message "Rebase target: %s" (majjic--rebase-target-mode-label target-mode)))

(defun majjic--rebase-set-source-mode (source-mode)
  "Set SOURCE-MODE in the current rebase state."
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (setf (majjic-rebase-state-source-mode majjic--rebase-state) source-mode)
  (setf (majjic-rebase-state-moved-change-ids majjic--rebase-state)
        (majjic--rebase-moved-change-ids
         (majjic-rebase-state-source-change-id majjic--rebase-state)
         source-mode))
  (majjic--sync-rebase-overlays)
  (message "Rebase source: %s"
           (if (eq source-mode 'descendants) "descendants" "revision")))

(defun majjic-section-forward ()
  "Move to the beginning of the next visible section.
Use Magit-style visible-section movement, but when starting from a non-auxiliary
section skip auxiliary top-level rows like the always-visible summary line and
elided/connector rows.  If a revision's file list is expanded, still move into
its first visible file section.
If point is between sections, recover by jumping to the next section at or
after point."
  (interactive)
  (let ((section (magit-section-at)))
    (cond
     ((null section)
      (if-let* ((next (majjic--next-section-from-point)))
          (magit-section-goto next)
        (user-error "No next section")))
     ((not (majjic--auxiliary-top-level-section-p section))
      (majjic--section-forward-skip-auxiliary))
     (t
      (magit-section-forward)))))

(defun majjic-section-backward ()
  "Move to the beginning of the previous visible section.
Use Magit-style visible-section movement, but when starting from a non-auxiliary
section skip auxiliary top-level rows like the always-visible summary line and
elided/connector rows.
If point is between sections, recover by jumping to the previous section at or
before point."
  (interactive)
  (let ((section (magit-section-at)))
    (cond
     ((null section)
      (if-let* ((prev (majjic--previous-section-from-point)))
          (magit-section-goto prev)
        (user-error "No previous section")))
     ((not (majjic--auxiliary-top-level-section-p section))
      (majjic--section-backward-skip-auxiliary))
     (t
      (magit-section-backward)))))

(defun majjic-section-up ()
  "Move to the parent section.
From a file row, skip the auxiliary summary section and jump to the revision
heading.  From hunks, move to the file section."
  (interactive)
  (let* ((section (or (magit-section-at)
                      (user-error "No section at point")))
         (parent (oref section parent)))
    (cond
     ((and (object-of-class-p section 'majjic-file-section)
           (object-of-class-p parent 'majjic-summary-section)
           (oref parent parent))
      (magit-section-goto (oref parent parent)))
     (parent
      (magit-section-goto parent))
     (t
      (user-error "No parent section")))))

(defun majjic-mwheel-scroll (event &optional arg)
  "Scroll with EVENT, then keep point off the edge in `majjic-log-mode'.
Use normal `mwheel-scroll' for the viewport, but if point lands on the first or
last visible line in the target `majjic' window, move it one line inward.  This
preserves normal non-conservative recentering for keyboard motion while
preventing redisplay from doing a larger recenter before the next wheel event."
  (interactive "e\nP")
  (let ((window (ignore-errors (posn-window (event-start event))))
        (direction (event-basic-type event)))
    (mwheel-scroll event arg)
    (when (and (window-live-p window)
               (buffer-live-p (window-buffer window)))
      (with-selected-window window
        (when (derived-mode-p 'majjic-log-mode)
          (majjic--nudge-point-from-wheel-edge direction))))))

(defun majjic--nudge-point-from-wheel-edge (direction)
  "Move point one line away from the edge DIRECTION is pushing toward."
  (let* ((column (current-column))
         (line (line-number-at-pos))
         (first-line (line-number-at-pos (window-start)))
         ;; `window-end' usually returns the buffer position just after the
         ;; last visible character, so its line number is often one past the
         ;; bottommost visible text line.
         (last-pos (window-end nil t))
         (last-line (line-number-at-pos (max (window-start)
                                             (1- last-pos)))))
    (pcase direction
      ((or 'wheel-up 'mouse-4)
       (when (and (= line last-line)
                  (> line first-line))
         (forward-line -1)
         (move-to-column column)))
      ((or 'wheel-down 'mouse-5)
       (when (and (= line first-line)
                  (< line last-line))
         (forward-line 1)
         (move-to-column column))))))

(defun majjic-toggle-at-point ()
  "Toggle the section at point.
On revisions and summaries, toggle the changed-file list.  On files and hunks,
toggle that section's body."
  (interactive)
  (let ((section (magit-section-at)))
    (cond
     ((null section)
      (user-error "No section at point"))
     ((or (object-of-class-p section 'majjic-file-section)
          (object-of-class-p section 'majjic-hunk-section))
      (when (and (object-of-class-p section 'majjic-file-section)
                 (not (oref section hidden)))
        (majjic--reset-section-load section))
      (when (and (object-of-class-p section 'majjic-hunk-section)
                 (oref section hidden))
        (majjic--prepare-missing-hunk-body section))
      (magit-section-toggle section))
     ((object-of-class-p section 'majjic-rename-section)
      (user-error "Rename rows do not expand yet"))
     (t
      (let* ((revision (majjic--current-revision-section))
             (summary (majjic--summary-child revision)))
        (unless summary
          (user-error "No section to toggle at point"))
        (unless (oref summary hidden)
          (majjic--reset-section-load summary))
        (magit-section-toggle summary)
        (magit-section-goto revision))))))

(defun majjic-visit-file ()
  "Visit the file or hunk location at point.
For file rows, visit the file.  For hunk headers and body lines, visit the
corresponding line in the new side, except removed lines open the old-side
snapshot at the deleted line.

For new-side visits in `@', or its unique parent when `@' is empty, visit the
working-tree file.  For older revisions, open a read-only snapshot buffer."
  (interactive)
  (let* ((file-section (majjic--current-file-section))
         (hunk-section (majjic--current-hunk-section)))
    (unless file-section
      (user-error "No file at point"))
    (let* ((commit-id (oref file-section commit-id))
           (path (oref file-section path)))
      (if hunk-section
          (pcase-let* ((`(,side ,line ,column) (majjic--hunk-location hunk-section)))
            (if (eq side 'old)
                (majjic--visit-file-snapshot
                 (or (majjic--single-parent-commit-id commit-id)
                     (user-error "Old-side navigation is only supported for single-parent revisions"))
                 path line column)
              (majjic--visit-file-new-side commit-id path line column)))
        (majjic--visit-file-new-side commit-id path nil)))))

(defun majjic--revert-buffer (&rest _ignore)
  "Refresh the current buffer for `revert-buffer'."
  (majjic-log-refresh))

(defun majjic--locate-root (dir)
  "Return the Jujutsu repository root for DIR, or nil if none."
  (condition-case nil
      (string-trim
       (majjic--call-jj dir "root"))
    (error
     (let ((default-workspace (expand-file-name "default" dir)))
       (when (file-directory-p (expand-file-name ".jj" default-workspace))
         default-workspace)))))

(defun majjic--log-buffer-name (repo-root)
  "Return the log buffer name for REPO-ROOT.
Follow Magit's naming style with a repo-specific buffer when possible."
  (if repo-root
      (format "majjic: %s"
              (file-name-nondirectory (directory-file-name repo-root)))
    "majjic"))

(defun majjic--get-log-buffer (repo-root)
  "Return the existing log buffer for REPO-ROOT, or create one."
  (or (majjic--find-log-buffer repo-root)
      (majjic--generate-log-buffer repo-root)))

(defun majjic--find-log-buffer (repo-root)
  "Return the existing Majjic log buffer for REPO-ROOT, if any."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'majjic-log-mode)
            (equal majjic--repo-root repo-root))))
   (buffer-list)))

(defun majjic--generate-log-buffer (repo-root)
  "Create a Majjic log buffer for REPO-ROOT.
When multiple repositories have the same basename, use Magit's
`uniquify' style and append distinguishing path components in
angle brackets."
  (let* ((default-directory (file-name-as-directory repo-root))
         (name (majjic--log-buffer-name repo-root))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq default-directory default-directory)
      (setq list-buffers-directory (abbreviate-file-name default-directory)))
    (majjic--maybe-uniquify-log-buffer-name buffer name repo-root)
    buffer))

(defun majjic--maybe-uniquify-log-buffer-name (buffer name repo-root)
  "Uniquify BUFFER named NAME using REPO-ROOT, following Magit."
  (cl-pushnew 'majjic-log-mode uniquify-list-buffers-directory-modes)
  (let ((uniquify-buffer-name-style
         (if (memq uniquify-buffer-name-style '(nil forward))
             'post-forward-angle-brackets
           uniquify-buffer-name-style)))
    (uniquify-rationalize-file-buffer-names
     name (file-name-directory (directory-file-name repo-root)) buffer)))

(defun majjic--capture-refresh-state ()
  "Capture the current transient UI state before a refresh."
  (let ((current (majjic--current-revision-section)))
    (make-majjic-state
     :current-commit-id (and current (oref current value))
     :current-change-id (and current (oref current change-id))
     :commit-change-ids (majjic--commit-change-ids)
     :expanded-commit-ids (majjic--expanded-commit-ids)
     :expanded-file-keys (majjic--expanded-file-keys)
     :marked-change-ids majjic--marked-change-ids
     :rebase-state majjic--rebase-state
     :squash-state majjic--squash-state)))

(defun majjic--log-refresh-async (state &optional generation)
  "Asynchronously refresh the current buffer, preserving UI STATE.
When GENERATION is non-nil, treat the refresh as part of an existing mutation
operation and ignore stale callbacks from older generations."
  (unless majjic--repo-root
    (user-error "Not inside a Jujutsu repository"))
  (when (and (majjic--operation-process-live-p majjic--refresh-process)
             (not generation))
    (delete-process majjic--refresh-process))
  (let ((refresh-generation (or generation
                                (setq majjic--process-generation
                                      (1+ majjic--process-generation))))
        process)
    (majjic--set-operation-status "Refreshing")
    (setq process
          (apply #'majjic--call-jj-capture-async majjic--repo-root
                 (lambda (exit-code stdout stderr)
                   (majjic--log-refresh-after-snapshot
                    process refresh-generation state exit-code stdout stderr))
                 (majjic--refresh-snapshot-args)))
    (setq majjic--refresh-process process)
    process))

(defun majjic--refresh-snapshot-args ()
  "Return args for the cheap jj command that snapshots before refresh."
  (list "status" "--quiet"))

(defun majjic--snapshot-before-refresh-sync ()
  "Ask jj to snapshot before a synchronous refresh."
  (pcase-let ((`(,exit-code ,stdout ,stderr)
               (apply #'majjic--call-jj-capture
                      majjic--repo-root
                      (majjic--refresh-snapshot-args))))
    (unless (zerop exit-code)
      (error "%s" (majjic--jj-error-message stdout stderr)))))

(defun majjic--log-refresh-after-snapshot
    (snapshot-process refresh-generation state exit-code stdout stderr)
  "Continue asynchronous refresh after SNAPSHOT-PROCESS exits."
  (when (eq snapshot-process majjic--refresh-process)
    (setq majjic--refresh-process nil))
  (when (= refresh-generation majjic--process-generation)
    (if (zerop exit-code)
        (majjic--start-log-refresh-read state refresh-generation)
      (majjic--render-refresh-error (majjic--jj-error-message stdout stderr))
      (majjic--finish-refresh refresh-generation))))

(defun majjic--start-log-refresh-read (state refresh-generation)
  "Start the read-only log process for asynchronous refresh."
  (let (process)
    (setq process
          (apply #'majjic--call-jj-capture-async majjic--repo-root
                 (lambda (exit-code stdout stderr)
                   (when (eq process majjic--refresh-process)
                     (setq majjic--refresh-process nil))
                   (when (= refresh-generation majjic--process-generation)
                     (if (zerop exit-code)
                         (condition-case err
                             (majjic--render-refreshed-records
                              (majjic--parse-log-output stdout) state)
                           (error
                            (majjic--render-refresh-error
                             (error-message-string err))))
                       (majjic--render-refresh-error
                        (majjic--jj-error-message stdout stderr))))
                   (majjic--finish-refresh refresh-generation))
                 (majjic--log-args)))
    (setq majjic--refresh-process process)
    process))

(defun majjic--render-refreshed-records (records state)
  "Render refreshed RECORDS, preserving UI STATE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (majjic--render-records records state)))

(defun majjic--render-refresh-error (message)
  "Replace the current buffer with refresh error MESSAGE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (majjic--render-error message)))

(defun majjic--finish-refresh (refresh-generation)
  "Clear async refresh and mutation state for REFRESH-GENERATION when current."
  (when (and (= refresh-generation majjic--process-generation)
             (not (majjic--operation-process-live-p majjic--refresh-process)))
    (setq majjic--mutation-in-progress nil)
    (setq majjic--mutation-process nil)
    (majjic--set-operation-status nil)))

(defun majjic--log-refresh-sync (state)
  "Synchronously snapshot and refresh, preserving UI STATE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (condition-case err
        (progn
          (majjic--snapshot-before-refresh-sync)
          (majjic--render-records (majjic--read-log-records) state))
      (error
       (majjic--render-error (error-message-string err))))))

(defun majjic--render-records (records state)
  "Render RECORDS and restore UI STATE."
  (setq state (majjic--remap-refresh-state records state))
  (setq majjic--render-generation (1+ majjic--render-generation))
  (setq majjic--marked-change-ids (majjic-state-marked-change-ids state))
  (setq majjic--abandon-selected-commit-ids nil)
  (setq majjic--rebase-state (majjic-state-rebase-state state))
  (setq majjic--squash-state (majjic-state-squash-state state))
  (when majjic--rebase-state
    (setf (majjic-rebase-state-moved-change-ids majjic--rebase-state)
          (majjic--rebase-moved-change-ids
           (majjic-rebase-state-source-change-id majjic--rebase-state)
           (or (majjic-rebase-state-source-mode majjic--rebase-state) 'revision))))
  (if records
      (progn
        (majjic--insert-records records
                                (majjic-state-expanded-commit-ids state)
                                (majjic-state-expanded-file-keys state))
        (majjic--goto-commit (majjic-state-current-commit-id state))
        (unless (majjic--current-revision-section)
          (goto-char (point-min))
          (when (magit-section-at)
            (magit-section-goto (magit-section-at))))
        (majjic--sync-mark-overlays)
        (majjic--sync-rebase-overlays)
        (majjic--sync-squash-overlays)
        (majjic--refresh-selection-highlights))
    (majjic--insert-message "No revisions to show.")))

(defun majjic--remap-refresh-state (records state)
  "Return STATE with commit ids remapped to matching change ids in RECORDS."
  (let ((remapped (copy-majjic-state state))
        (commit-ids (majjic--record-commit-ids records))
        (change-commit-ids (majjic--record-change-commit-ids records)))
    (setf (majjic-state-current-commit-id remapped)
          (majjic--remap-state-commit
           (majjic-state-current-commit-id state)
           (majjic-state-current-change-id state)
           commit-ids change-commit-ids))
    (setf (majjic-state-expanded-commit-ids remapped)
          (majjic--remap-state-commits
           (majjic-state-expanded-commit-ids state)
           (majjic-state-commit-change-ids state)
           commit-ids change-commit-ids))
    (setf (majjic-state-expanded-file-keys remapped)
          (majjic--remap-state-file-keys
           (majjic-state-expanded-file-keys state)
           (majjic-state-commit-change-ids state)
           commit-ids change-commit-ids))
    remapped))

(defun majjic--record-commit-ids (records)
  "Return visible revision commit ids from parsed log RECORDS."
  (let (ids)
    (dolist (record records)
      (when (eq (majjic-revision-kind record) 'revision)
        (push (majjic-revision-commit-id record) ids)))
    ids))

(defun majjic--record-change-commit-ids (records)
  "Return an alist of (CHANGE-ID . COMMIT-ID) from parsed log RECORDS."
  (let (ids)
    (dolist (record records)
      (when (eq (majjic-revision-kind record) 'revision)
        (push (cons (majjic-revision-change-id record)
                    (majjic-revision-commit-id record))
              ids)))
    ids))

(defun majjic--remap-state-commit (commit-id change-id commit-ids change-commit-ids)
  "Remap COMMIT-ID by CHANGE-ID if it disappeared from COMMIT-IDS."
  (or (and (member commit-id commit-ids)
           commit-id)
      (and change-id
           (cdr (assoc change-id change-commit-ids)))
      commit-id))

(defun majjic--remap-state-commits
    (commit-ids old-commit-change-ids new-commit-ids new-change-commit-ids)
  "Remap COMMIT-IDS using old and new commit/change alists."
  (delq nil
        (delete-dups
         (mapcar
          (lambda (commit-id)
            (majjic--remap-state-commit
             commit-id
             (cdr (assoc commit-id old-commit-change-ids))
             new-commit-ids new-change-commit-ids))
          commit-ids))))

(defun majjic--remap-state-file-keys
    (keys old-commit-change-ids new-commit-ids new-change-commit-ids)
  "Remap expanded file KEYS using old and new commit/change alists."
  (delq nil
        (delete-dups
         (mapcar
          (lambda (key)
            (when-let* ((commit-id (majjic--remap-state-commit
                                    (car key)
                                    (cdr (assoc (car key) old-commit-change-ids))
                                    new-commit-ids new-change-commit-ids)))
              (cons commit-id (cdr key))))
          keys))))

(defun majjic--refresh-selection-highlights ()
  "Refresh section highlights after rendering outside `post-command-hook'."
  (magit-section-update-highlight t)
  (majjic--update-selected-hunk-heading))

(defun majjic--render-error (message)
  "Render error MESSAGE in the current buffer."
  (setq majjic--render-generation (1+ majjic--render-generation))
  (majjic--insert-message message))

(defun majjic--revision-commit-ids ()
  "Return commit ids for visible revision sections in buffer order."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (object-of-class-p child 'majjic-revision-section)
          (push (oref child value) ids))))
    (nreverse ids)))

(defun majjic--restyle-abandon-revision (revision commit-id)
  "Update abandon styling overlay for REVISION in place."
  (when revision
    (let ((inhibit-read-only t))
      (majjic--delete-abandon-overlay commit-id)
      (when (member commit-id majjic--abandon-selected-commit-ids)
        (let ((overlay (make-overlay (oref revision start) (oref revision end) nil t t)))
          (overlay-put overlay 'majjic-abandon-commit-id commit-id)
          (overlay-put overlay 'face 'majjic-abandon-included-row)
          (push overlay majjic--abandon-overlays))))))

(defun majjic--sync-abandon-overlays ()
  "Rebuild abandon overlays for all selected revisions in the current buffer."
  (majjic--clear-abandon-overlays)
  (when (bound-and-true-p magit-root-section)
    (dolist (revision (oref magit-root-section children))
      (when (and (object-of-class-p revision 'majjic-revision-section)
                 (member (oref revision value) majjic--abandon-selected-commit-ids))
        (majjic--restyle-abandon-revision revision (oref revision value))))))

(defun majjic--delete-abandon-overlay (commit-id)
  "Delete the abandon overlay for COMMIT-ID, if present."
  (setq majjic--abandon-overlays
        (cl-remove-if (lambda (overlay)
                        (when (equal (overlay-get overlay 'majjic-abandon-commit-id) commit-id)
                          (delete-overlay overlay)
                          t))
                      majjic--abandon-overlays)))

(defun majjic--clear-abandon-overlays ()
  "Remove all abandon styling overlays from the current buffer."
  (mapc #'delete-overlay majjic--abandon-overlays)
  (setq majjic--abandon-overlays nil))

(defun majjic--marked-visible-commit-ids ()
  "Return visible commit ids corresponding to persistent marked change ids."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when (and (object-of-class-p revision 'majjic-revision-section)
                   (member (oref revision change-id) majjic--marked-change-ids))
          (push (oref revision value) ids))))
    (nreverse ids)))

(defun majjic--marked-visible-change-ids ()
  "Return visible marked change ids in buffer order."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when (and (object-of-class-p revision 'majjic-revision-section)
                   (member (oref revision change-id) majjic--marked-change-ids))
          (push (oref revision change-id) ids))))
    (nreverse ids)))

(defun majjic--rebase-source-change-ids (source)
  "Normalize SOURCE to a list of change ids."
  (cond
   ((null source) nil)
   ((listp source) source)
   (t (list source))))

(defun majjic--refresh-rebase-source-from-marks ()
  "If rebase mode is active, refresh the source set from visible marks.
When there are no visible marks, keep the current staged source unchanged."
  (when (and majjic-rebase-mode majjic--rebase-state)
    (when-let* ((visible-marked (majjic--marked-visible-change-ids)))
      (setf (majjic-rebase-state-source-change-id majjic--rebase-state)
            (if (= (length visible-marked) 1)
                (car visible-marked)
              visible-marked))
      (setf (majjic-rebase-state-moved-change-ids majjic--rebase-state)
            (majjic--rebase-moved-change-ids
             (majjic-rebase-state-source-change-id majjic--rebase-state)
             (or (majjic-rebase-state-source-mode majjic--rebase-state) 'revision)))
      (majjic--sync-rebase-overlays))))

(defun majjic--sync-mark-overlays ()
  "Rebuild persistent mark overlays for visible marked revisions."
  (majjic--clear-mark-overlays)
  (when (bound-and-true-p magit-root-section)
    (dolist (revision (oref magit-root-section children))
      (when (and (object-of-class-p revision 'majjic-revision-section)
                 (member (oref revision change-id) majjic--marked-change-ids))
        (majjic--restyle-marked-revision revision)))))

(defun majjic--restyle-marked-revision (revision)
  "Update the persistent mark overlay for REVISION in place."
  (when revision
    (let* ((change-id (oref revision change-id))
           (pos (save-excursion
                  (magit-section-goto revision)
                  (line-beginning-position))))
      (majjic--delete-mark-overlay change-id)
      (when (member change-id majjic--marked-change-ids)
        (let* ((selected-revision (magit-current-section))
               (selectedp (eq revision selected-revision))
               (display-face (if selectedp
                                 '(:inherit (majjic-marked-row magit-section-highlight))
                               'majjic-marked-row))
               (overlay (make-overlay pos (min (point-max) (1+ pos)) nil t t)))
          (overlay-put overlay 'display
                       (propertize "*" 'face display-face))
          (overlay-put overlay 'priority 1002)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'majjic-mark-change-id change-id)
          (push overlay majjic--mark-overlays))))))

(defun majjic--delete-mark-overlay (change-id)
  "Delete the persistent mark overlay for CHANGE-ID, if present."
  (setq majjic--mark-overlays
        (cl-remove-if (lambda (overlay)
                        (when (equal (overlay-get overlay 'majjic-mark-change-id) change-id)
                          (delete-overlay overlay)
                          t))
                      majjic--mark-overlays)))

(defun majjic--clear-mark-overlays ()
  "Remove all persistent mark overlays from the current buffer."
  (mapc #'delete-overlay majjic--mark-overlays)
  (setq majjic--mark-overlays nil))

(defun majjic--next-revision-section (section)
  "Return the next revision sibling after SECTION, skipping auxiliary rows."
  (let ((next (majjic--next-top-level-section section)))
    (while (and next (not (object-of-class-p next 'majjic-revision-section)))
      (setq next (majjic--next-top-level-section next)))
    next))

(defun majjic--expanded-commit-ids ()
  "Return commit ids whose file sections are currently expanded."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when-let* ((summary (majjic--summary-child revision)))
          (when (not (oref summary hidden))
            (push (oref revision value) ids)))))
    ids))

(defun majjic--commit-change-ids ()
  "Return (COMMIT-ID . CHANGE-ID) pairs for currently visible revisions."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when (object-of-class-p revision 'majjic-revision-section)
          (push (cons (oref revision value) (oref revision change-id)) ids))))
    ids))

(defun majjic--expanded-file-keys ()
  "Return (COMMIT-ID . PATH) pairs for currently expanded file sections."
  (let (keys)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when-let* ((summary (majjic--summary-child revision)))
          (when (not (oref summary hidden))
            (dolist (child (oref summary children))
              (when (and (object-of-class-p child 'majjic-file-section)
                         (not (oref child hidden)))
                (push (cons (oref child commit-id) (oref child path)) keys)))))))
    keys))

(defun majjic--goto-commit (commit-id)
  "Move point to COMMIT-ID if present in the current buffer."
  (when (and commit-id (bound-and-true-p magit-root-section))
    (when-let* ((section (seq-find (lambda (child)
                                     (and (object-of-class-p child 'majjic-revision-section)
                                          (equal (oref child value) commit-id)))
                                   (oref magit-root-section children))))
      (magit-section-goto section))))

(defun majjic--hunk-location (hunk)
  "Return (SIDE LINE COLUMN) for HUNK at point.
SIDE is `new' for added/context lines and `old' for removed lines.
When point is on the hunk header, use the first changed body position on the
new side; only actual removed body lines visit the old side."
  (save-excursion
    (let ((headerp (< (point) (oref hunk content))))
      (when headerp
        (goto-char (oref hunk content))
        (unless (majjic--advance-to-first-changed-hunk-line hunk)
          (user-error "No changed line in hunk")))
      (majjic--hunk-location-at-point hunk (and headerp 'new)))))

(defun majjic--advance-to-first-changed-hunk-line (hunk)
  "Move point to the first added or removed line in HUNK.
Return non-nil if one was found."
  (let ((end (oref hunk end))
        found)
    (while (and (not found) (< (point) end))
      (let ((kind (majjic--current-hunk-line-kind hunk)))
        (if (memq kind '(old new))
            (setq found t)
          (forward-line 1))))
    found))

(defun majjic--hunk-location-at-point (hunk &optional preferred-side)
  "Compute the diff side, line number, and source column for point inside HUNK.
When PREFERRED-SIDE is non-nil, return that side's line number even if point is
currently on the opposite side.  This is used for hunk headers, which should
open the selected revision rather than inheriting an initial removal line."
  (let ((old (or (oref hunk old-start)
                 (user-error "Cannot parse hunk header")))
        (new (or (oref hunk new-start)
                 (user-error "Cannot parse hunk header")))
        (target-line (line-beginning-position))
        ;; Capture the user's horizontal position before walking from the hunk
        ;; start to count old/new line numbers.  The scan below leaves point at
        ;; BOL, so computing the column afterward would always return 0.
        (source-column (majjic--current-hunk-source-column hunk))
        side
        line)
    (save-excursion
      (goto-char (oref hunk content))
      (while (and (< (point) target-line)
                  (< (point) (oref hunk end)))
        (pcase (majjic--current-hunk-line-kind hunk)
          ('old (setq old (1+ old)))
          ('new (setq new (1+ new)))
          ('context (setq old (1+ old)
                          new (1+ new)))
          (_ nil))
        (forward-line 1))
      (setq side
            (or preferred-side
                (pcase (majjic--current-hunk-line-kind hunk)
                  ('old 'old)
                  (_ 'new))))
      (setq line (if (eq side 'old) old new)))
    (list side line source-column)))

(defun majjic--current-hunk-source-column (hunk)
  "Return the source column corresponding to point on the current hunk line.
If point is still in the graph gutter or diff marker, use column 0."
  (let* ((body-prefix (or (oref hunk body-prefix) ""))
         (marker-column (+ (length body-prefix) 1))
         (offset (- (current-column) marker-column)))
    (max 0 offset)))

(defun majjic--current-hunk-line-kind (hunk)
  "Return the kind of the current line in HUNK.
The result is one of `old', `new', `context', `meta', or nil."
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
         (body-prefix (or (oref hunk body-prefix) ""))
         (plain (majjic--strip-ansi line))
         (diff-line (if (string-prefix-p body-prefix plain)
                        (substring plain (length body-prefix))
                      plain)))
    (pcase (and (> (length diff-line) 0) (aref diff-line 0))
      (?- 'old)
      (?+ 'new)
      (?\s 'context)
      (?\\ 'meta)
      (_ nil))))

(defun majjic--visit-file-new-side (commit-id path &optional line column)
  "Visit PATH from the new side of COMMIT-ID, optionally at LINE and COLUMN.
Use the working tree for `@' and its empty-parent special case, otherwise open a
read-only snapshot."
  (let ((worktree-path (expand-file-name path majjic--repo-root)))
    (if (and (majjic--working-tree-commit-p commit-id)
             (file-exists-p worktree-path))
        (progn
          (find-file worktree-path)
          (majjic--goto-line-column line column))
      (majjic--visit-file-snapshot commit-id path line column))))

(defun majjic--visit-file-snapshot (commit-id path &optional line column)
  "Open PATH from COMMIT-ID in a read-only buffer, optionally at LINE and COLUMN."
  (let* ((content (majjic--call-jj majjic--repo-root "file" "show"
                                  "--color" "never" "--no-pager"
                                  "--ignore-working-copy" "-r" commit-id "--" path))
         (short (substring commit-id 0 (min 8 (length commit-id))))
         (buffer (get-buffer-create (format "*majjic %s:%s*" short path)))
         (worktree-path (expand-file-name path majjic--repo-root))
         (worktree-dir (file-name-directory worktree-path))
         window)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (setq default-directory
              (if (file-exists-p worktree-dir)
                  worktree-dir
                majjic--repo-root))
        (majjic--snapshot-normal-mode worktree-path)
        (goto-char (point-min))
        (majjic--goto-line-column line column)
        (setq-local buffer-read-only t)))
    ;; Reuse an existing window for this snapshot buffer when possible and set
    ;; point after the buffer is displayed.  That avoids `pop-to-buffer'
    ;; restoring a stale window-point when reopening the same temporary buffer.
    (setq window (or (get-buffer-window buffer 0)
                     (display-buffer buffer)))
    (when (window-live-p window)
      (select-window window)
      (with-current-buffer buffer
        (majjic--goto-line-column line column)
        (set-window-point window (point))))))

(defun majjic--snapshot-normal-mode (file)
  "Select FILE's usual major mode while keeping snapshot-only behavior.
Snapshot buffers are not actually visiting FILE, but temporarily binding
`buffer-file-name' lets `normal-mode' choose the same mode it would use for the
working-tree file."
  (let ((buffer-file-name file)
        (after-change-major-mode-hook
         ;; Snapshot buffers should feel like file buffers without waking up
         ;; editor integrations that expect a real writable visit.
         (seq-difference after-change-major-mode-hook
                         '(global-diff-hl-mode-enable-in-buffer
                           global-diff-hl-mode-enable-in-buffers
                           eglot--maybe-activate-editing-mode)
                         #'eq)))
    ;; Match Magit's blob buffers: respect nil `enable-local-variables' while
    ;; still letting file names, shebangs, and file-local variables pick a mode.
    (normal-mode (not enable-local-variables))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (majjic-snapshot-mode 1)))

(defun majjic--goto-line-column (&optional line column)
  "Move to LINE and COLUMN in the current buffer when provided."
  (when line
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (when column
      (move-to-column column))))

(defun majjic-debug-display-at-point ()
  "Report the rendered face and overlay state at point.
This is meant for diagnosing visual issues where the final display differs from
the text properties produced by `majjic' helpers."
  (interactive)
  (let* ((library (locate-library "majjic"))
         (section (ignore-errors (magit-current-section)))
         (info (list :library library
                     :buffer (buffer-name)
                     :point (point)
                     :line (line-number-at-pos)
                     :column (current-column)
                     :char (char-after)
                     :text-face (get-text-property (point) 'face)
                     :text-font-lock-face (get-text-property (point) 'font-lock-face)
                     :face-at-point (face-at-point nil t)
                     :overlays (mapcar (lambda (overlay)
                                         (list :start (overlay-start overlay)
                                               :end (overlay-end overlay)
                                               :priority (overlay-get overlay 'priority)
                                               :face (overlay-get overlay 'face)))
                                       (overlays-at (point)))
                     :section-type (and section (eieio-object-class-name section))
                     :section-hidden (and section (slot-boundp section 'hidden)
                                          (oref section hidden)))))
    (if (called-interactively-p 'interactive)
        (message "%S" info)
      info)))

(provide 'majjic)

;;; majjic.el ends here
