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

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)
(require 'ansi-color)
(require 'color)

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

(defvar-local majjic--last-error nil
  "Last refresh error shown in the current `majjic-log-mode' buffer.")

(defvar-local majjic--mutation-in-progress nil
  "Non-nil while a mutating Jujutsu command is running in this buffer.")

(defvar-local majjic--abandon-selected-ids nil
  "List of commit ids selected in abandon mode.")

(defvar-local majjic--abandon-overlays nil
  "Overlays used to style revisions selected in abandon mode.")

(defvar-local majjic--records nil
  "Last parsed log records rendered in the current buffer.")

(defvar-local majjic--selected-hunk-heading-overlays nil
  "Overlays used to emphasize hunk headers in the selected file section.")

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
  (add-hook 'post-command-hook #'majjic--update-selected-hunk-heading nil t))

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

(keymap-set majjic-log-mode-map "n" #'majjic-section-forward)
(keymap-set majjic-log-mode-map "p" #'majjic-section-backward)
(keymap-set majjic-log-mode-map "^" #'majjic-section-up)
(keymap-set majjic-log-mode-map "N" #'majjic-new)
(keymap-set majjic-log-mode-map "e" #'majjic-edit)
(keymap-set majjic-log-mode-map "a" #'majjic-abandon-start)
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
    (let ((current-change-id (majjic--current-change-id))
          (expanded-change-ids (majjic--expanded-change-ids))
          (expanded-file-keys (majjic--expanded-file-keys)))
      (majjic--log-refresh-sync current-change-id expanded-change-ids expanded-file-keys)))

(defun majjic-new ()
  "Create a new child of the current revision and move to `@'."
  (interactive)
  (when majjic-abandon-mode
    (majjic-abandon-disabled-command))
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
  (let ((commit-id (majjic--require-current-commit-id)))
    (majjic--run-mutation
     (lambda ()
       (majjic--call-jj majjic--repo-root "edit" "-r" commit-id))
     :target #'majjic--working-copy-commit-id)))

(defun majjic-abandon-start ()
  "Enter abandon mode with no revisions selected yet."
  (interactive)
  (when majjic-abandon-mode
    (user-error "Already in abandon mode"))
  (setq majjic--abandon-selected-ids nil)
  (majjic-abandon-mode 1)
  (message "Abandon mode: SPC toggle, RET apply, C-g cancel"))

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

(defun majjic--read-log-records ()
  "Return parsed revision records for the current buffer's repository."
  (unless majjic--repo-root
    (error "Not inside a Jujutsu repository"))
  (majjic--parse-log-output (apply #'majjic--call-jj majjic--repo-root (majjic--log-args))))

(defun majjic--log-refresh-sync (current-change-id expanded-change-ids expanded-file-keys)
  "Synchronously refresh, preserving CURRENT-CHANGE-ID and expansion state."
  (let ((inhibit-read-only t))
    (setq majjic--last-error nil)
    (erase-buffer)
    (condition-case err
        (majjic--render-records (majjic--read-log-records) current-change-id
                               expanded-change-ids expanded-file-keys)
      (error
       (majjic--render-error (error-message-string err))))))

(defun majjic--render-records (records current-change-id expanded-change-ids expanded-file-keys)
  "Render RECORDS and restore CURRENT-CHANGE-ID and EXPANDED-CHANGE-IDS."
  (setq majjic--records records)
  (if records
      (progn
        (majjic--insert-records records expanded-change-ids expanded-file-keys)
        (majjic--goto-change current-change-id)
        (unless (majjic--current-revision-section)
          (goto-char (point-min))
          (when (magit-section-at)
            (magit-section-goto (magit-section-at))))
        (majjic--sync-abandon-overlays))
    (majjic--insert-message "No revisions to show.")))

(defun majjic--render-error (message)
  "Render error MESSAGE in the current buffer."
  (setq majjic--records nil)
  (setq majjic--last-error message)
  (majjic--insert-message message))

(defun majjic--log-args ()
  "Build arguments for the `jj log' process."
  (append
   (list "log" "--color" "always" "--no-pager" "--ignore-working-copy"
         "--template" (majjic--combined-template))
   (when majjic-log-revset
     (list "--revisions" majjic-log-revset))
   (when majjic-log-limit
     (list "--limit" (number-to-string majjic-log-limit)))))

(defun majjic--combined-template ()
  "Build the Jujutsu template used to render one revision record."
  (concat "\"" majjic--record-separator "\" ++ change_id ++ \""
          majjic--record-separator "\" ++ commit_id ++ \""
          majjic--record-separator "\" ++ builtin_log_comfortable"))

(defun majjic--call-jj (dir &rest args)
  "Run `majjic-program' in DIR with ARGS and return stdout.
Signal an error if the process exits non-zero or the executable is missing."
  (unless (executable-find majjic-program)
    (error "Cannot find `%s' in PATH" majjic-program))
  (let ((stderr-file (make-temp-file "majjic-jj-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory dir))
            (let ((exit-code (apply #'process-file majjic-program nil
                                    (list (current-buffer) stderr-file) nil args))
                  (stdout (buffer-string)))
              (if (zerop exit-code)
                  stdout
                (let ((stderr (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-string))))
                  (error "%s" (string-trim (if (string-blank-p stderr)
                                               stdout
                                             stderr))))))))
      (ignore-errors (delete-file stderr-file)))))

(defun majjic--run-mutation (thunk &rest plist)
  "Run mutating THUNK with refresh and simple in-flight protection.
Keyword args:
:target is a commit id or a function returning one after success.
:after-success is a function called before refreshing after success."
  (when majjic--mutation-in-progress
    (user-error "Another jj mutation is already running"))
  (unless majjic--repo-root
    (user-error "Not inside a Jujutsu repository"))
  (let* ((majjic--mutation-in-progress t)
         (target-spec (plist-get plist :target))
         (after-success (plist-get plist :after-success))
         (current-change-id (majjic--current-change-id))
         (expanded-change-ids (majjic--expanded-change-ids))
         (expanded-file-keys (majjic--expanded-file-keys))
         (target current-change-id))
    (unwind-protect
        (condition-case err
            (progn
              (funcall thunk)
              (when after-success
                (funcall after-success))
              (setq target (cond
                            ((functionp target-spec) (funcall target-spec))
                            (target-spec target-spec)
                            (t current-change-id)))
              (majjic--log-refresh-sync target expanded-change-ids expanded-file-keys))
          (error
           (message "%s" (error-message-string err))
           nil))
      (setq majjic--mutation-in-progress nil))))

(defun majjic--require-current-commit-id ()
  "Return the current revision commit id, or signal a user error."
  (or (majjic--current-change-id)
      (user-error "No revision selected")))

(defun majjic--working-copy-commit-id ()
  "Return the commit id for `@' in the current repository view."
  (string-trim
   (majjic--call-jj majjic--repo-root "log" "-r" "@"
                   "--ignore-working-copy" "--no-graph"
                   "--color" "never" "--template" "commit_id")))

(defun majjic--revision-exists-p (commit-id)
  "Return non-nil if COMMIT-ID exists in the current repo view."
  (when commit-id
    (condition-case nil
        (not (string-empty-p
              (string-trim
               (majjic--call-jj majjic--repo-root "log" "-r" commit-id
                               "--ignore-working-copy" "--no-graph"
                               "--color" "never" "--template" "commit_id"))))
      (error nil))))

(defun majjic--selection-fallbacks (commit-id)
  "Return candidate revisions to select if COMMIT-ID vanishes after mutation."
  (let ((ids (majjic--revision-commit-ids)))
    (when-let* ((pos (cl-position commit-id ids :test #'equal)))
      (delq nil (list (nth (1+ pos) ids)
                      (nth (1- pos) ids)
                      (majjic--working-copy-commit-id))))))

(defun majjic--revision-commit-ids ()
  "Return commit ids for visible revision sections in buffer order."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (object-of-class-p child 'majjic-revision-section)
          (push (oref child value) ids))))
    (nreverse ids)))

(defun majjic--prefixed-rev-args (commit-ids)
  "Return `-r' args for COMMIT-IDS."
  (cl-mapcan (lambda (commit-id) (list "-r" commit-id)) commit-ids))

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

(defun majjic--refresh-abandon-display ()
  "Refresh abandon overlays in the current buffer."
  (majjic--sync-abandon-overlays))

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

(defun majjic--revision-heading-text (record)
  "Return propertized heading text for revision RECORD."
  (let* ((commit-id (plist-get record :commit-id))
         (heading (majjic--ansi-colorize (plist-get record :heading))))
    (ignore commit-id)
    heading))

(defun majjic--revision-summary-text (record)
  "Return propertized summary text for revision RECORD."
  (majjic--ansi-colorize (plist-get record :summary)))

(defun majjic--body-text (string commit-id)
  "Return propertized body STRING for COMMIT-ID, preserving color."
  (ignore commit-id)
  (majjic--ansi-colorize string))

(defconst majjic--summary-status-labels
  '(("A" . "added")
    ("C" . "copied")
    ("D" . "deleted")
    ("M" . "modified")
    ("R" . "renamed")
    ("T" . "typechanged"))
  "Human-readable labels for `jj diff --summary' status codes.")

(defun majjic--insert-file-heading (string &optional color-source)
  "Insert file heading STRING with a colorized status marker.
Render the path itself in the default face so file boundaries stay distinct from
hunk text, then overlay the diff status indicator after `magit-insert-heading'
so the color survives section rendering and current-row highlighting.
When COLOR-SOURCE is non-nil, sample the status color from that string instead
of STRING.  This is useful when STRING has a widened human-readable status
label, but the original summary output only colored the short status code."
  (let* ((colored (majjic--ansi-colorize (or color-source string)))
         (text (majjic--strip-ansi string))
         status-beg status-end status-face)
    (when (> (length text) 0)
      (put-text-property 0 (length text) 'face 'default text))
    (when (string-match "\\`[[:space:]│├└┼┤┬╯╮╭╰─]*\\(\\S-+\\)\\([[:space:]]+\\)" text)
      (setq status-beg (match-beginning 1))
      (setq status-end (match-end 1))
      (setq status-face (or (get-text-property status-beg 'face colored)
                            (get-text-property status-beg 'font-lock-face colored))))
    (let ((heading-start (point)))
      (magit-insert-heading text)
      (when status-face
        (let ((overlay (make-overlay (+ heading-start status-beg)
                                     (+ heading-start status-end))))
          (overlay-put overlay 'face (majjic--status-marker-face status-face))
          (overlay-put overlay 'priority 1002)
          (overlay-put overlay 'evaporate t))))))

(defun majjic--status-marker-face (face)
  "Return FACE with bold weight for summary status markers."
  (if (listp face)
      (append face '(:weight bold))
    (list :inherit face :weight 'bold)))

(defun majjic--summary-status-label (status)
  "Return the human-readable label for summary STATUS."
  (if (stringp status)
      (or (alist-get status majjic--summary-status-labels nil nil #'string=)
          (downcase status))
    ""))

(defun majjic--summary-status-token (line)
  "Return the summary status token from LINE, or nil if none."
  (let ((plain (string-trim-left (majjic--strip-ansi line))))
    (car (split-string plain "[[:space:]]+" t))))

(defun majjic--summary-status-width ()
  "Return the fixed width for human-readable summary status labels."
  (apply #'max (mapcar (lambda (pair) (length (cdr pair)))
                       majjic--summary-status-labels)))

(defun majjic--format-summary-line (line width)
  "Return summary LINE with a padded human-readable status column of WIDTH.
Preserve ANSI escapes from the path portion of LINE."
  (let ((plain (majjic--strip-ansi line)))
    (if (string-match "\\`\\([[:space:]│├└┼┤┬╯╮╭╰─]*\\)\\(\\S-+\\)\\([[:space:]]+\\)\\(.*\\)\\'" plain)
        (let* ((prefix (match-string 1 plain))
               (status (match-string 2 plain))
               (label (majjic--summary-status-label status))
               (path (match-string 4 plain)))
          (format "%s%s %s" prefix (string-pad label width) path))
      line)))

(defun majjic--apply-hunk-heading-background (start end)
  "Overlay a darker background on the hunk heading from START to END.
Use an overlay instead of text properties because Magit section rendering can
discard appended face properties on the final inserted heading text."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face (majjic--hunk-heading-face))
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun majjic--hunk-heading-face (&optional selected)
  "Return a theme-relative face for hunk headings.
Make the background slightly darker than `magit-section-highlight'.  When
SELECTED is non-nil, add bold weight."
  (let* ((highlight-bg (face-attribute 'magit-section-highlight :background nil t))
         (background (when (majjic--usable-color-p highlight-bg)
                       (condition-case nil
                           (color-darken-name highlight-bg 8)
                         (error nil)))))
    (append (list :inherit 'magit-section-highlight :extend t)
            (when background
              (list :background background))
            (when selected
              (list :weight 'bold)))))

(defun majjic--usable-color-p (color)
  "Return non-nil if COLOR is a concrete color string."
  (and (stringp color)
       (not (member color '("unspecified" "unspecified-bg" "unspecified-fg")))))

(defun majjic--update-selected-hunk-heading ()
  "Bold hunk headers according to the currently selected section.
Revision and summary sections bold all visible hunk headers beneath that
revision, file sections bold all hunks in that file, and hunk sections bold only
the current hunk."
  (dolist (overlay majjic--selected-hunk-heading-overlays)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq majjic--selected-hunk-heading-overlays nil)
  (dolist (hunk (majjic--selected-hunk-sections))
    (when (and (markerp (oref hunk start))
               (markerp (oref hunk content)))
      (save-excursion
        (goto-char (oref hunk start))
        (let ((overlay (make-overlay (line-beginning-position)
                                     (min (point-max) (1+ (line-end-position))))))
          (overlay-put overlay 'face (majjic--hunk-heading-face t))
          (overlay-put overlay 'priority 1001)
          (overlay-put overlay 'evaporate t)
          (push overlay majjic--selected-hunk-heading-overlays))))))

(defun majjic--selected-hunk-sections ()
  "Return hunk sections that should be emphasized for the current selection."
  (when-let* ((section (magit-current-section)))
    (cond
     ((object-of-class-p section 'majjic-hunk-section)
      (list section))
     ((object-of-class-p section 'majjic-file-section)
      (majjic--descendant-hunk-sections section))
     ((object-of-class-p section 'majjic-summary-section)
      (majjic--descendant-hunk-sections section))
     ((object-of-class-p section 'majjic-revision-section)
      (when-let* ((summary (majjic--summary-child section)))
        (majjic--descendant-hunk-sections summary))))))

(defun majjic--descendant-hunk-sections (section)
  "Return all descendant hunk sections beneath SECTION."
  (let (hunks)
    (dolist (child (oref section children))
      (cond
       ((object-of-class-p child 'majjic-hunk-section)
        (push child hunks))
       (t
        (setq hunks (nconc (nreverse (majjic--descendant-hunk-sections child))
                           hunks)))))
    (nreverse hunks)))

(defun majjic--parse-log-output (output)
  "Parse `jj log --summary' OUTPUT into revision records."
  (let ((lines (split-string output "\n"))
        records
        current
        expecting-summary)
    (dolist (line lines)
      (cond
       ((majjic--parse-heading-line line)
        (when current
          (push (nreverse current) records))
        (setq current (list (majjic--parse-heading-line line)))
        (setq expecting-summary t))
       ((majjic--elided-line-p line)
        (when current
          (push (nreverse current) records))
        (push (list (cons 'elided line)) records)
        (setq current nil)
        (setq expecting-summary nil))
       ((and (not expecting-summary) (majjic--graph-continuation-line-p line))
        (when current
          (push (nreverse current) records))
        (push (list (cons 'graph line)) records)
        (setq current nil)
        (setq expecting-summary nil))
       ((and current expecting-summary)
        (push (cons 'summary (majjic--normalize-summary-line line)) current)
        (setq expecting-summary nil))
       ((and current (not (string-empty-p line)))
        ;; Ignore any extra lines from the visible log template. File bodies are
        ;; loaded lazily on expansion instead of being pre-rendered into the
        ;; buffer.
        nil)))
    (when current
      (push (nreverse current) records))
    (nreverse (mapcar #'majjic--record-alist-to-plist records))))

(defun majjic--parse-heading-line (line)
  "If LINE starts a revision record, return an alist entry for it."
  (let ((regexp (concat "^\\(.*?\\)" (regexp-quote majjic--record-separator)
                        "\\([^" (regexp-quote majjic--record-separator) "]+\\)"
                        (regexp-quote majjic--record-separator)
                        "\\([^" (regexp-quote majjic--record-separator) "]+\\)"
                        (regexp-quote majjic--record-separator)
                        "\\(.*\\)$")))
    (when (string-match regexp line)
      (let ((prefix (match-string 1 line))
            (change-id (match-string 2 line))
            (commit-id (match-string 3 line))
            (text (match-string 4 line)))
        (cons 'heading
              (list :change-id (majjic--strip-ansi change-id)
                    :commit-id (majjic--strip-ansi commit-id)
                    :text (concat prefix text)))))))

(defun majjic--record-alist-to-plist (record)
  "Convert parsed RECORD alist into a plist."
  (if-let* ((elided (alist-get 'elided record)))
      (list :kind 'elided :heading elided)
    (if-let* ((graph (alist-get 'graph record)))
        (list :kind 'graph :heading graph)
      (let* ((heading-entry (alist-get 'heading record))
             (commit-id (plist-get heading-entry :commit-id))
             (summary (or (alist-get 'summary record) "")))
        ;; The synthetic root revision has no description line in `jj log', but
        ;; the trailing newline can otherwise be normalized into our placeholder.
        (when (and (majjic--root-commit-id-p commit-id)
                   (string-match-p "(no description set)" summary))
          (setq summary nil))
      (list :kind 'revision
            :change-id (plist-get heading-entry :change-id)
            :commit-id commit-id
            :heading (plist-get heading-entry :text)
            :summary summary)))))

(defun majjic--insert-records (records expanded-change-ids expanded-file-keys)
  "Insert RECORDS, restoring expanded revisions and files from saved state."
  (magit-insert-section (majjic-root-section)
    (dolist (record records)
      (pcase (plist-get record :kind)
        ('elided
         (magit-insert-section (majjic-elided-section)
           (magit-insert-heading (majjic--ansi-colorize (plist-get record :heading)))))
        ('graph
         (magit-insert-section (majjic-graph-line-section)
           (magit-insert-heading (majjic--ansi-colorize (plist-get record :heading)))))
        (_
         (let* ((commit-id (plist-get record :commit-id))
               (expanded (member commit-id expanded-change-ids)))
           (magit-insert-section (majjic-revision-section commit-id)
             (magit-insert-heading (majjic--revision-heading-text record))
             (when-let* ((summary (plist-get record :summary)))
               (magit-insert-section (majjic-summary-section commit-id (not expanded))
                 (magit-insert-heading (majjic--ansi-colorize summary))
                 (magit-insert-section-body
                   (majjic--insert-file-summary commit-id expanded-file-keys)))))))))))

(defun majjic--insert-message (message)
  "Insert MESSAGE as plain buffer contents."
  (insert message)
  (unless (bolp)
    (insert "\n"))
  (goto-char (point-min)))

(defun majjic--current-revision-section ()
  "Return the revision section at point, or its revision ancestor."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'majjic-revision-section)))
      (setq section (oref section parent)))
    section))

(defun majjic--current-file-section ()
  "Return the file section at point, or its file ancestor."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'majjic-file-section)))
      (setq section (oref section parent)))
    section))

(defun majjic--current-hunk-section ()
  "Return the hunk section at point, or nil."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'majjic-hunk-section)))
      (setq section (oref section parent)))
    section))

(defun majjic--current-change-id ()
  "Return the current revision's commit id, or nil."
  (when-let* ((section (majjic--current-revision-section)))
    (oref section value)))

(defun majjic--summary-child (revision)
  "Return the summary child section for REVISION."
  (when revision
    (seq-find (lambda (child)
                (object-of-class-p child 'majjic-summary-section))
              (oref revision children))))

(defun majjic--next-section-from-point ()
  "Return the next section at or after point, or nil."
  (let ((pos (point))
        section)
    (while (and (< pos (point-max))
                (not (setq section (magit-section-at pos))))
      (setq pos (or (next-single-property-change pos 'magit-section nil (point-max))
                    (point-max))))
    section))

(defun majjic--previous-section-from-point ()
  "Return the previous section at or before point, or nil."
  (let ((pos (min (point) (1- (point-max))))
        section)
    (while (and (> pos (point-min))
                (not (setq section (magit-section-at pos))))
      (setq pos (or (previous-single-property-change pos 'magit-section nil (point-min))
                    (point-min))))
    (or section (magit-section-at pos))))

(defun majjic--auxiliary-top-level-section-p (section)
  "Return non-nil if SECTION is a top-level row to skip from revision headings."
  (or (object-of-class-p section 'majjic-summary-section)
      (object-of-class-p section 'majjic-elided-section)
      (object-of-class-p section 'majjic-graph-line-section)))

(defun majjic--section-forward-skip-auxiliary ()
  "Move forward, skipping auxiliary rows unless already on one."
  (let ((start (point)))
    (condition-case err
        (progn
          (magit-section-forward)
          (while (majjic--auxiliary-top-level-section-p (magit-section-at))
            (magit-section-forward)))
      (user-error
       (goto-char start)
       (signal (car err) (cdr err))))))

(defun majjic--section-backward-skip-auxiliary ()
  "Move backward, skipping auxiliary rows unless already on one."
  (let ((start (point)))
    (condition-case err
        (progn
          (magit-section-backward)
          (while (majjic--auxiliary-top-level-section-p (magit-section-at))
            (magit-section-backward)))
      (user-error
       (goto-char start)
       (signal (car err) (cdr err))))))

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

(defun majjic--elided-line-p (line)
  "Return non-nil if LINE is Jujutsu's synthetic elided-revisions row."
  (string-match-p "(elided revisions)" line))

(defun majjic--graph-continuation-line-p (line)
  "Return non-nil if LINE is a graph-only junction row from `jj log'.
Preserve merge/elbow rows like \"│ ├─╯\", but drop plain vertical spacer rows
like \"│\" or \"│ │\" to avoid adding extra blank space between revisions."
  (let ((plain (string-trim (majjic--strip-ansi line))))
    (and (not (string-empty-p plain))
         (not (string-match-p "[[:alnum:]]" plain))
         (or (string= plain "~")
             (string-match-p "[├└┼┤┬╯╮╭╰─]" plain)))))

(defun majjic--ansi-colorize (string)
  "Return STRING with ANSI escapes converted to text properties."
  (let ((colored (ansi-color-apply string))
        (pos 0)
        (has-face nil))
    (while (< pos (length colored))
      (let* ((next (next-single-property-change pos 'font-lock-face colored
                                                (length colored)))
             (font-lock-face (get-text-property pos 'font-lock-face colored)))
        (when font-lock-face
          (setq has-face t)
          (put-text-property pos next 'face font-lock-face colored))
        (setq pos next)))
    ;; If there was no ANSI color, force a non-nil face so Magit doesn't apply
    ;; `magit-section-heading' to the whole line.
    (unless has-face
      (put-text-property 0 (length colored) 'face 'default colored))
    colored))

(defun majjic--strip-ansi (string)
  "Return STRING with ANSI escape sequences removed."
  (with-temp-buffer
    (insert string)
    (ansi-color-filter-region (point-min) (point-max))
    (buffer-string)))

(defun majjic--insert-file-summary (commit-id expanded-file-keys)
  "Insert a lazily loaded file summary for COMMIT-ID.
Restore expanded file diffs listed in EXPANDED-FILE-KEYS."
  (let* ((prefix (majjic--file-summary-prefix))
         (output (string-trim-right
                  (majjic--call-jj majjic--repo-root "diff" "--summary"
                                  "--color" "always" "-r" commit-id)))
         (lines (unless (string-empty-p output)
                  (split-string output "\n")))
         (status-width (majjic--summary-status-width)))
    (if (string-empty-p output)
        (insert (majjic--body-text (concat prefix "(no files changed)") commit-id) "\n")
      (dolist (line lines)
        (let ((heading-line (concat prefix (majjic--format-summary-line line status-width)))
              (color-source (concat prefix line)))
        (if (majjic--rename-summary-line-p line)
            (magit-insert-section (majjic-rename-section nil nil :commit-id commit-id)
              (majjic--insert-file-heading heading-line color-source))
          (let* ((path (majjic--summary-line-path line))
                 (expanded (member (cons commit-id path) expanded-file-keys)))
            (magit-insert-section (majjic-file-section (cons commit-id path) (not expanded)
                                                      :commit-id commit-id
                                                      :path path)
              (majjic--insert-file-heading heading-line color-source)
              (magit-insert-section-body
                (majjic--insert-file-diff commit-id path))))))))))

(defun majjic--insert-file-diff (commit-id path)
  "Insert hunk sections for PATH in COMMIT-ID."
  (let* ((prefix (majjic--file-summary-prefix))
         (output (string-trim-right
                  (majjic--call-jj majjic--repo-root "diff" "--git"
                                  "--color" "always" "-r" commit-id "--" path)))
         (lines (if (string-empty-p output) nil (split-string output "\n")))
         (hunk-header nil)
         (hunk-body nil)
         (saw-hunk nil)
         (hunk-index 0))
    (cl-labels ((flush-hunk ()
                           (when hunk-header
                             (let ((body-lines (nreverse hunk-body)))
                               (pcase-let* ((`(,old-start ,old-count ,new-start ,new-count)
                                            (or (majjic--parse-hunk-header hunk-header)
                                                (list nil nil nil nil))))
                                 (setq saw-hunk t)
                                 ;; Give each hunk a unique value so Magit can
                                 ;; track nested visibility independently.
                                 (magit-insert-section (majjic-hunk-section hunk-index nil
                                                                           :old-start old-start
                                                                           :old-count old-count
                                                                           :new-start new-start
                                                                           :new-count new-count
                                   :body-prefix prefix
                                   :body-lines body-lines)
                                   (let ((heading-start (point)))
                                     (magit-insert-heading
                                      (majjic--body-text (concat prefix hunk-header) commit-id))
                                     (majjic--apply-hunk-heading-background
                                      heading-start (point)))
                                   (magit-insert-section-body
                                     (majjic--insert-hunk-body-lines prefix body-lines commit-id)))
                                 (setq hunk-index (1+ hunk-index))
                                 (setq hunk-header nil)
                                 (setq hunk-body nil))))))
      (dolist (line lines)
        (if (majjic--hunk-header-line-p line)
            (progn
              (flush-hunk)
              (setq hunk-header line))
          (when hunk-header
            (push line hunk-body))))
      (flush-hunk)
      (unless saw-hunk
        (insert (majjic--body-text (concat prefix "(no diff)") commit-id) "\n")))))

(defun majjic--insert-hunk-body-lines (prefix body-lines commit-id)
  "Insert BODY-LINES for a hunk using PREFIX and preserve color.
Apply abandon styling for COMMIT-ID when relevant."
  (dolist (body-line body-lines)
    (insert (majjic--body-text (concat prefix body-line) commit-id) "\n")))

(defun majjic--prepare-missing-hunk-body (hunk)
  "If HUNK lost its body text while hidden, regenerate it on the next show.
`magit-insert-section-body' only installs a washer for sections that start out
hidden.  Our hunks start expanded, so if a hidden hunk's body disappears after
an ancestor toggle, restore a one-shot washer right before reopening it."
  (when (and (= (oref hunk content) (oref hunk end))
             (null (oref hunk washer))
             (oref hunk body-lines))
    (let ((prefix (or (oref hunk body-prefix) ""))
          (body-lines (oref hunk body-lines))
          (commit-id (when-let* ((file (oref hunk parent)))
                       (and (object-of-class-p file 'majjic-file-section)
                            (oref file commit-id)))))
      (oset hunk washer
            (lambda ()
              (majjic--insert-hunk-body-lines prefix body-lines commit-id))))))

(defun majjic--file-summary-prefix ()
  "Return the graph gutter prefix to use for lazily loaded file rows."
  (let* ((summary magit-insert-section--current)
         (heading (if summary
                      (buffer-substring-no-properties (oref summary start)
                                                      (oref summary content))
                    "")))
    (if (string-match "\\`\\([^[:alnum:]]*\\)" heading)
        (majjic--continuation-prefix (match-string 1 heading))
      "")))

(defun majjic--continuation-prefix (prefix)
  "Turn graph heading PREFIX into a continuation prefix for child rows.
Keep vertical bars and spaces, and replace other graph glyphs with spaces so
files align under the revision while preserving branch columns."
  (apply #'string
         (mapcar (lambda (char)
                   (cond
                    ((eq char ?│) ?│)
                    ;; Elbows/tees with a downward segment should continue as a
                    ;; vertical line through expanded child rows.
                    ((memq char '(?├ ?┼ ?┤ ?┬ ?┌ ?┐ ?╭ ?╮)) ?│)
                    ((eq char ?\s) ?\s)
                    ((eq char ?\t) ?\t)
                    (t ?\s)))
                 (string-to-list prefix))))

(defun majjic--normalize-summary-line (line)
  "Ensure blank summary LINE still shows a placeholder description."
  (if (string-match-p "[[:alnum:]]" line)
      line
    (concat line "(no description set)")))

(defun majjic--root-commit-id-p (commit-id)
  "Return non-nil if COMMIT-ID is Jujutsu's synthetic root commit."
  (and (stringp commit-id)
       (string-match-p "\\`0+\\'" commit-id)))

(defun majjic--summary-line-path (line)
  "Extract a path from a `jj diff --summary' LINE."
  (let ((plain (majjic--strip-ansi line)))
    (if (string-match "\\`[^[:space:]]+[[:space:]]+\\(.+\\)\\'" plain)
        (majjic--normalize-summary-path (match-string 1 plain))
      plain)))

(defun majjic--rename-summary-line-p (line)
  "Return non-nil if summary LINE is a rename row.
Rename summaries like \"R {old => new}\" are display-only for now because they
do not expand cleanly to a single path."
  (let ((plain (majjic--strip-ansi line)))
    (string-match-p "\\`R[[:space:]]+{.+ => .+}\\'" plain)))

(defun majjic--normalize-summary-path (path)
  "Return the likely working-tree path from summary PATH text."
  (if (string-match " -> \\(.+\\)\\'" path)
      (match-string 1 path)
    path))

(defun majjic--hunk-header-line-p (line)
  "Return non-nil if LINE is a unified diff hunk header."
  (string-prefix-p "@@ " (majjic--strip-ansi line)))

(defun majjic--parse-hunk-header (line)
  "Parse unified diff hunk header LINE.
Return a list (OLD-START OLD-COUNT NEW-START NEW-COUNT), or nil if LINE does not
look like a hunk header."
  (let ((plain (majjic--strip-ansi line)))
    (when (string-match
           "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
           plain)
      (list (string-to-number (match-string 1 plain))
            (string-to-number (or (match-string 2 plain) "1"))
            (string-to-number (match-string 3 plain))
            (string-to-number (or (match-string 4 plain) "1"))))))

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

(defun majjic--working-tree-commit-p (commit-id)
  "Return non-nil if COMMIT-ID should visit the working-tree file."
  (member commit-id (majjic--working-tree-commit-ids)))

(defun majjic--working-tree-commit-ids ()
  "Return commit ids that should open files from the working tree."
  (let* ((current (string-trim
                   (majjic--call-jj majjic--repo-root "log" "-r" "@"
                                   "--ignore-working-copy"
                                   "--no-graph" "--color" "never"
                                   "--template" "commit_id")))
         (ids (list current))
         (current-summary (string-trim
                           (majjic--call-jj majjic--repo-root "diff" "--summary"
                                           "--ignore-working-copy"
                                           "--color" "never" "-r" "@"))))
    (when (string-empty-p current-summary)
      (let* ((parents-output (string-trim
                              (majjic--call-jj majjic--repo-root "log" "-r" "parents(@)"
                                              "--ignore-working-copy"
                                              "--no-graph" "--color" "never"
                                              "--template" "commit_id ++ \"\\n\"")))
             (parents (seq-remove #'string-empty-p (split-string parents-output "\n"))))
        (when (= (length parents) 1)
          (push (car parents) ids))))
    ids))

(defun majjic--single-parent-commit-id (commit-id)
  "Return the unique parent commit id of COMMIT-ID, or nil if not unique."
  (let* ((parents-output (string-trim
                          (majjic--call-jj majjic--repo-root "log"
                                          "-r" (format "parents(%s)" commit-id)
                                          "--no-graph" "--color" "never"
                                          "--template" "commit_id ++ \"\\n\"")))
         (parents (seq-remove #'string-empty-p (split-string parents-output "\n"))))
    (when (= (length parents) 1)
      (car parents))))

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
