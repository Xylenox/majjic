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
(require 'subr-x)
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

(defconst majjic--record-separator "\x1f"
  "Separator used to mark revision records in `jj log' output.")

(defvar-local majjic--repo-root nil
  "Repository root for the current `majjic-log-mode' buffer.")

(defvar-local majjic--mutation-in-progress nil
  "Non-nil while a mutating Jujutsu command is running in this buffer.")

(defvar-local majjic--abandon-selected-ids nil
  "List of commit ids selected in abandon mode.")

(defvar-local majjic--abandon-overlays nil
  "Overlays used to style revisions selected in abandon mode.")

(defvar-local majjic--selected-hunk-heading-overlays nil
  "Overlays used to emphasize hunk headers in the selected file section.")

(defvar-local majjic--rebase-state nil
  "Transient state for `majjic-rebase-mode'.")

(defvar-local majjic--rebase-overlays nil
  "Overlays used to preview rebase source and target in `majjic-rebase-mode'.")

(defconst majjic--op-preview-max-height 10
  "Maximum height for the temporary undo/redo operation preview window.")

(require 'majjic-jj)
(require 'majjic-render)
(require 'majjic-actions)

(declare-function majjic--clear-rebase-overlays "majjic-render")
(declare-function majjic--change-id-for-commit-id "majjic-jj")
(declare-function majjic--commit-id-for-change-id "majjic-jj")
(declare-function majjic--effective-redo-operation-preview "majjic-jj")
(declare-function majjic--effective-undo-operation-preview "majjic-jj")
(declare-function majjic--op-log-preview "majjic-jj")
(declare-function majjic--redo-args "majjic-jj")
(declare-function majjic--rebase-target-mode-label "majjic-render")
(declare-function majjic--sync-rebase-overlays "majjic-render")
(declare-function majjic--undo-args "majjic-jj")

(defface majjic-abandon-included-row
  '((t :inherit shadow :strike-through t))
  "Face for revisions included in abandon mode."
  :group 'majjic)

(defclass majjic-revision-section (magit-section)
  ((keymap :initform 'majjic-revision-section-map)))

(defclass majjic-summary-section (magit-section)
  ((keymap :initform 'majjic-summary-section-map)))

(defclass majjic-file-section (magit-section)
  ((keymap :initform 'majjic-file-section-map)
   (commit-id :initarg :commit-id)
   (path :initarg :path)))

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

(defvar-keymap majjic-section-map
  :parent magit-section-mode-map
  "TAB" #'majjic-toggle-at-point
  "SPC" #'majjic-space
  "n" #'majjic-section-forward
  "p" #'majjic-section-backward
  "^" #'majjic-section-up
  "N" #'majjic-new
  "e" #'majjic-edit
  "a" #'majjic-abandon-start
  "r" #'majjic-rebase-start
  "u" #'majjic-undo
  "U" #'majjic-redo
  "<wheel-up>" #'majjic-mwheel-scroll
  "<wheel-down>" #'majjic-mwheel-scroll
  "<mouse-4>" #'majjic-mwheel-scroll
  "<mouse-5>" #'majjic-mwheel-scroll
  "g" #'majjic-log-refresh)

(define-key majjic-section-map [32] #'majjic-space)

(defvar-keymap majjic-abandon-mode-map
  "SPC" #'majjic-abandon-toggle-revision
  "a" #'majjic-abandon-start
  "RET" #'majjic-abandon-apply
  "<return>" #'majjic-abandon-apply
  "C-g" #'majjic-abandon-cancel)

(define-key majjic-abandon-mode-map " " #'majjic-abandon-toggle-revision)
(define-key majjic-abandon-mode-map [remap scroll-up-command]
            #'majjic-abandon-toggle-revision)

(defvar-keymap majjic-rebase-mode-map
  "o" #'majjic-rebase-set-onto
  "a" #'majjic-rebase-set-after
  "b" #'majjic-rebase-set-before
  "RET" #'majjic-rebase-apply
  "<return>" #'majjic-rebase-apply
  "C-g" #'majjic-rebase-cancel)

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
  (add-hook 'post-command-hook #'majjic--sync-rebase-overlays nil t))

(defvar-keymap majjic-snapshot-mode-map
  :parent special-mode-map
  "q" #'quit-window)

(define-derived-mode majjic-snapshot-mode special-mode "Majjic-Snapshot"
  "Major mode for read-only `majjic' snapshot buffers.
\\<majjic-snapshot-mode-map>\\[quit-window] closes the temporary snapshot window."
  :group 'majjic)

(define-minor-mode majjic-abandon-mode
  "Minor mode for staging `jj abandon' selections in a `majjic' log buffer."
  :lighter " Abandon"
  :keymap majjic-abandon-mode-map
  (unless (derived-mode-p 'majjic-log-mode)
    (setq majjic-abandon-mode nil)
    (user-error "Not in a majjic log buffer"))
  (unless majjic-abandon-mode
    (setq majjic--abandon-selected-ids nil)
    (majjic--clear-abandon-overlays))
  (force-mode-line-update))

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

(keymap-set majjic-log-mode-map "n" #'majjic-section-forward)
(keymap-set majjic-log-mode-map "p" #'majjic-section-backward)
(keymap-set majjic-log-mode-map "^" #'majjic-section-up)
(keymap-set majjic-log-mode-map "N" #'majjic-new)
(keymap-set majjic-log-mode-map "e" #'majjic-edit)
(keymap-set majjic-log-mode-map "a" #'majjic-abandon-start)
(keymap-set majjic-log-mode-map "r" #'majjic-rebase-start)
(keymap-set majjic-log-mode-map "u" #'majjic-undo)
(keymap-set majjic-log-mode-map "U" #'majjic-redo)
(keymap-set majjic-log-mode-map "<wheel-up>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<wheel-down>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<mouse-4>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "<mouse-5>" #'majjic-mwheel-scroll)
(keymap-set majjic-log-mode-map "g" #'majjic-log-refresh)

(defun majjic-space ()
  "In abandon mode toggle the revision at point, otherwise scroll up.
This is bound on section keymaps so it wins over inherited Magit
section bindings that would otherwise page the buffer."
  (interactive)
  (if majjic-abandon-mode
      (majjic-abandon-toggle-revision)
    (call-interactively #'scroll-up-command)))

(defun majjic ()
  "Open a Jujutsu log buffer for the current repository."
  (interactive)
  (let* ((source-dir default-directory)
         (repo-root (majjic--locate-root source-dir))
         (buffer (get-buffer-create (majjic--log-buffer-name repo-root))))
    (with-current-buffer buffer
      (majjic-log-mode)
      (setq default-directory source-dir)
      (setq majjic--repo-root repo-root)
      (majjic-log-refresh))
    (pop-to-buffer buffer)))

(defun majjic-log-refresh ()
  "Refresh the current Jujutsu log buffer."
  (interactive)
  (unless (derived-mode-p 'majjic-log-mode)
    (user-error "Not in a majjic log buffer"))
  (majjic--log-refresh-sync (majjic--capture-refresh-state)))

(defun majjic-new ()
  "Create a new child of the current revision and move to `@'."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (let ((commit-id (majjic--require-current-commit-id)))
    (majjic--run-mutation
     (lambda ()
       (majjic--call-jj majjic--repo-root "new" commit-id))
     :target #'majjic--working-copy-commit-id)))

(defun majjic-edit ()
  "Edit the current revision and move to `@'."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (let ((commit-id (majjic--require-current-commit-id)))
    (majjic--run-mutation
     (lambda ()
       (majjic--call-jj majjic--repo-root "edit" "-r" commit-id))
     :target #'majjic--working-copy-commit-id)))

(defun majjic-undo ()
  "Undo the latest Jujutsu operation after confirmation."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (majjic--run-confirmed-op-mutation
   "undo"
   (lambda ()
     (apply #'majjic--call-jj majjic--repo-root (majjic--undo-args)))))

(defun majjic-redo ()
  "Redo the latest undone Jujutsu operation after confirmation."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
  (when majjic-rebase-mode
    (majjic-rebase-disabled-command))
  (majjic--run-confirmed-op-mutation
   "redo"
   (lambda ()
     (apply #'majjic--call-jj majjic--repo-root (majjic--redo-args)))))

(defun majjic-abandon-start ()
  "Enter abandon mode with no revisions selected yet."
  (interactive)
  (if majjic-rebase-mode
      (majjic-rebase-set-after)
    (when majjic-abandon-mode
      (user-error "Already in abandon mode"))
    (setq majjic--abandon-selected-ids nil)
    (majjic-abandon-mode 1)
    (message "Abandon mode: SPC toggle, RET apply, C-g cancel")))

(defun majjic-abandon-toggle-revision ()
  "Toggle direct abandon selection for the current revision."
  (interactive)
  (majjic--abandon-toggle-current))

(defun majjic-abandon-apply ()
  "Apply the current abandon selection."
  (interactive)
  (unless majjic-abandon-mode
    (user-error "Not in abandon mode"))
  (let* ((selected-ids majjic--abandon-selected-ids)
         (fallbacks (majjic--selection-fallbacks (majjic--current-change-id))))
    (when (null selected-ids)
      (user-error "No revisions selected for abandon"))
    (majjic--run-mutation
     (lambda ()
       (apply #'majjic--call-jj majjic--repo-root
              (append (list "abandon" "--retain-bookmarks")
                      (majjic--prefixed-rev-args selected-ids))))
     :after-success (lambda ()
                      (majjic-abandon-mode -1))
     :target (lambda ()
               (or (seq-find #'majjic--revision-exists-p fallbacks)
                   (majjic--working-copy-commit-id))))))

(defun majjic-abandon-cancel ()
  "Cancel abandon mode without mutating the repository."
  (interactive)
  (unless majjic-abandon-mode
    (user-error "Not in abandon mode"))
  (majjic-abandon-mode -1)
  (message "Abandon canceled"))

(defun majjic-abandon-disabled-command ()
  "Reject commands that are unavailable while staging abandon selections."
  (interactive)
  (user-error "Disabled in abandon mode"))

(defun majjic-rebase-start ()
  "Enter rebase mode using the current revision as the source."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
  (when majjic-rebase-mode
    (user-error "Already in rebase mode"))
  (setq majjic--rebase-state
        (make-majjic-rebase-state
         :source-change-id (majjic--change-id-for-commit-id
                            (majjic--require-current-commit-id))
         :target-mode 'onto))
  (majjic-rebase-mode 1)
  (message "Rebase mode: o onto, a after, b before, RET apply, C-g cancel"))

(defun majjic-rebase-set-onto ()
  "Set the rebase target placement to onto."
  (interactive)
  (majjic--rebase-set-target-mode 'onto))

(defun majjic-rebase-set-after ()
  "Set the rebase target placement to after."
  (interactive)
  (majjic--rebase-set-target-mode 'after))

(defun majjic-rebase-set-before ()
  "Set the rebase target placement to before."
  (interactive)
  (majjic--rebase-set-target-mode 'before))

(defun majjic-rebase-apply ()
  "Apply the staged rebase."
  (interactive)
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (let* ((source (majjic-rebase-state-source-change-id majjic--rebase-state))
         (target (majjic--require-current-commit-id))
         (target-change-id (majjic--change-id-for-commit-id target))
         (target-mode (majjic-rebase-state-target-mode majjic--rebase-state)))
    (when (equal source target-change-id)
      (user-error "Rebase source and target are the same revision"))
    (majjic--run-mutation
     (lambda ()
       (apply #'majjic--call-jj majjic--repo-root
              (majjic--rebase-args source target target-mode)))
     :after-success (lambda ()
                      (majjic-rebase-mode -1))
     :target (lambda ()
               (or (majjic--commit-id-for-change-id source)
                   (majjic--working-copy-commit-id))))))

(defun majjic-rebase-cancel ()
  "Cancel rebase mode without mutating the repository."
  (interactive)
  (unless majjic-rebase-mode
    (user-error "Not in rebase mode"))
  (majjic-rebase-mode -1)
  (message "Rebase canceled"))

(defun majjic-rebase-disabled-command ()
  "Reject commands that are unavailable while staging a rebase."
  (interactive)
  (user-error "Disabled in rebase mode"))

(defun majjic--run-confirmed-op-mutation (action thunk)
  "Preview the latest operation, confirm ACTION, and run mutating THUNK."
  (let* ((current (majjic--current-change-id))
         (fallbacks (majjic--selection-fallbacks current)))
    (when (majjic--confirm-latest-operation action)
      (majjic--run-mutation
       thunk
       :target (lambda ()
                 (or (and current (majjic--revision-exists-p current) current)
                     (seq-find #'majjic--revision-exists-p fallbacks)
                     (majjic--working-copy-commit-id)))))))

(defun majjic--confirm-latest-operation (action)
  "Show the effective operation preview in a side window and ask to perform ACTION."
  (let* ((preview (pcase action
                    ("undo" (majjic--effective-undo-operation-preview))
                    ("redo" (majjic--effective-redo-operation-preview))
                    (_ (majjic--op-log-preview))))
         (buffer (get-buffer-create (format "*majjic %s preview*" action)))
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
          (y-or-n-p (format "%s latest change? " (capitalize action))))
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
      (when (and (object-of-class-p section 'majjic-hunk-section)
                 (oref section hidden))
        (majjic--prepare-missing-hunk-body section))
      (magit-section-toggle section)
      (when-let* ((revision (and majjic-abandon-mode (majjic--current-revision-section)))
                  (commit-id (oref revision value)))
        (majjic--restyle-abandon-revision revision commit-id)))
     ((object-of-class-p section 'majjic-rename-section)
      (user-error "Rename rows do not expand yet"))
     (t
      (let* ((revision (majjic--current-revision-section))
             (summary (majjic--summary-child revision)))
        (unless summary
          (user-error "No section to toggle at point"))
        (magit-section-toggle summary)
        (when majjic-abandon-mode
          (majjic--restyle-abandon-revision revision (oref revision value)))
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

(defun majjic--capture-refresh-state ()
  "Capture the current transient UI state before a refresh."
  (make-majjic-state
   :current-change-id (majjic--current-change-id)
   :expanded-change-ids (majjic--expanded-change-ids)
   :expanded-file-keys (majjic--expanded-file-keys)
   :abandon-selected-ids majjic--abandon-selected-ids
   :rebase-state majjic--rebase-state))

(defun majjic--log-refresh-sync (state)
  "Synchronously refresh, preserving UI STATE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (condition-case err
        (majjic--render-records (majjic--read-log-records) state)
      (error
       (majjic--render-error (error-message-string err))))))

(defun majjic--render-records (records state)
  "Render RECORDS and restore UI STATE."
  (setq majjic--abandon-selected-ids (majjic-state-abandon-selected-ids state))
  (setq majjic--rebase-state (majjic-state-rebase-state state))
  (if records
      (progn
        (majjic--insert-records records
                                (majjic-state-expanded-change-ids state)
                                (majjic-state-expanded-file-keys state))
        (majjic--goto-change (majjic-state-current-change-id state))
        (unless (majjic--current-revision-section)
          (goto-char (point-min))
          (when (magit-section-at)
            (magit-section-goto (magit-section-at))))
        (majjic--sync-abandon-overlays)
        (majjic--sync-rebase-overlays))
    (majjic--insert-message "No revisions to show.")))

(defun majjic--render-error (message)
  "Render error MESSAGE in the current buffer."
  (majjic--insert-message message))

(defun majjic--revision-commit-ids ()
  "Return commit ids for visible revision sections in buffer order."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (object-of-class-p child 'majjic-revision-section)
          (push (oref child value) ids))))
    (nreverse ids)))

(defun majjic--abandon-toggle-current ()
  "Toggle current revision in the abandon selection."
  (unless majjic-abandon-mode
    (user-error "Not in abandon mode"))
  (let ((commit-id (majjic--require-current-commit-id))
        (revision (majjic--current-revision-section)))
    (setq majjic--abandon-selected-ids
          (if (member commit-id majjic--abandon-selected-ids)
              (cl-remove commit-id majjic--abandon-selected-ids :test #'equal)
            (cons commit-id majjic--abandon-selected-ids)))
    (majjic--restyle-abandon-revision revision commit-id)))

(defun majjic--restyle-abandon-revision (revision commit-id)
  "Update abandon styling overlay for REVISION in place."
  (when revision
    (let ((inhibit-read-only t))
      (majjic--delete-abandon-overlay commit-id)
      (when (member commit-id majjic--abandon-selected-ids)
        (let ((overlay (make-overlay (oref revision start) (oref revision end) nil t t)))
          (overlay-put overlay 'majjic-abandon-commit-id commit-id)
          (overlay-put overlay 'face 'majjic-abandon-included-row)
          (push overlay majjic--abandon-overlays))))))

(defun majjic--sync-abandon-overlays ()
  "Rebuild abandon overlays for all selected revisions in the current buffer."
  (when majjic-abandon-mode
    (majjic--clear-abandon-overlays)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when (and (object-of-class-p revision 'majjic-revision-section)
                   (member (oref revision value) majjic--abandon-selected-ids))
          (majjic--restyle-abandon-revision revision (oref revision value)))))))

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

(defun majjic--expanded-change-ids ()
  "Return commit ids whose file sections are currently expanded."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when-let* ((summary (majjic--summary-child revision)))
          (when (not (oref summary hidden))
            (push (oref revision value) ids)))))
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

(defun majjic--goto-change (commit-id)
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
When point is on the hunk header, use the first changed line in the body."
  (save-excursion
    (when (< (point) (oref hunk content))
      (goto-char (oref hunk content))
      (unless (majjic--advance-to-first-changed-hunk-line hunk)
        (user-error "No changed line in hunk")))
    (majjic--hunk-location-at-point hunk)))

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

(defun majjic--hunk-location-at-point (hunk)
  "Compute the diff side, line number, and source column for point inside HUNK."
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
      (pcase (majjic--current-hunk-line-kind hunk)
        ('old (setq side 'old
                    line old))
        ('new (setq side 'new
                    line new))
        ('context (setq side 'new
                        line new))
        (_ (setq side 'new
                 line new))))
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
         window)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (majjic-snapshot-mode)
        (erase-buffer)
        (insert content)
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
