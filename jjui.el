;;; jjui.el --- Minimal Jujutsu log UI -*- lexical-binding: t; -*-

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

(defgroup jjui nil
  "Minimal UI for Jujutsu."
  :group 'tools
  :prefix "jjui-")

(defcustom jjui-program "jj"
  "Executable used to run Jujutsu commands."
  :type 'string
  :group 'jjui)

(defcustom jjui-log-revset nil
  "Revset passed to `jj log'.
When nil, use Jujutsu's default log revset."
  :type '(choice (const :tag "Default" nil)
                 string)
  :group 'jjui)

(defcustom jjui-log-limit nil
  "Maximum number of revisions to show in `jjui-log'.
When nil, do not pass a limit to `jj log'."
  :type '(choice (const :tag "No limit" nil)
                 integer)
  :group 'jjui)

(defconst jjui--record-separator "\x1f"
  "Separator used to mark revision records in `jj log' output.")

(defvar-local jjui--repo-root nil
  "Repository root for the current `jjui-log-mode' buffer.")

(defvar-local jjui--last-error nil
  "Last refresh error shown in the current `jjui-log-mode' buffer.")

(defvar-local jjui--mutation-in-progress nil
  "Non-nil while a mutating Jujutsu command is running in this buffer.")

(defvar-local jjui--abandon-selected-ids nil
  "List of commit ids selected in abandon mode.")

(defvar-local jjui--abandon-overlays nil
  "Overlays used to style revisions selected in abandon mode.")

(defvar-local jjui--records nil
  "Last parsed log records rendered in the current buffer.")

(defface jjui-abandon-included-row
  '((t :inherit shadow :strike-through t))
  "Face for revisions included in abandon mode."
  :group 'jjui)

(defclass jjui-revision-section (magit-section)
  ((keymap :initform 'jjui-revision-section-map)))

(defclass jjui-summary-section (magit-section)
  ((keymap :initform 'jjui-summary-section-map)))

(defclass jjui-file-section (magit-section)
  ((keymap :initform 'jjui-file-section-map)
   (commit-id :initarg :commit-id)
   (path :initarg :path)))

(defclass jjui-hunk-section (magit-section)
  ((keymap :initform 'jjui-hunk-section-map)
   (old-start :initarg :old-start)
   (old-count :initarg :old-count)
   (new-start :initarg :new-start)
   (new-count :initarg :new-count)
   (body-prefix :initarg :body-prefix)
   (body-lines :initarg :body-lines)))

(defclass jjui-root-section (magit-section) ())

(defclass jjui-elided-section (magit-section)
  ((keymap :initform 'jjui-summary-section-map)))

(defclass jjui-graph-line-section (magit-section)
  ((keymap :initform 'jjui-summary-section-map)))

(defvar-keymap jjui-section-map
  :parent magit-section-mode-map
  "TAB" #'jjui-toggle-at-point
  "SPC" #'jjui-space
  "n" #'jjui-section-forward
  "p" #'jjui-section-backward
  "^" #'jjui-section-up
  "N" #'jjui-new
  "e" #'jjui-edit
  "a" #'jjui-abandon-start
  "<wheel-up>" #'jjui-mwheel-scroll
  "<wheel-down>" #'jjui-mwheel-scroll
  "<mouse-4>" #'jjui-mwheel-scroll
  "<mouse-5>" #'jjui-mwheel-scroll
  "g" #'jjui-log-refresh)

(define-key jjui-section-map [32] #'jjui-space)

(defvar-keymap jjui-abandon-mode-map
  "SPC" #'jjui-abandon-toggle-revision
  "a" #'jjui-abandon-start
  "RET" #'jjui-abandon-apply
  "<return>" #'jjui-abandon-apply
  "C-g" #'jjui-abandon-cancel)

(define-key jjui-abandon-mode-map " " #'jjui-abandon-toggle-revision)
(define-key jjui-abandon-mode-map [remap scroll-up-command]
            #'jjui-abandon-toggle-revision)

(defvar-keymap jjui-revision-section-map
  :parent jjui-section-map)

(defvar-keymap jjui-summary-section-map
  :parent jjui-section-map)

(defvar-keymap jjui-file-section-map
  :parent jjui-section-map
  "RET" #'jjui-visit-file
  "<return>" #'jjui-visit-file)

(defvar-keymap jjui-hunk-section-map
  :parent jjui-section-map
  "RET" #'jjui-visit-file
  "<return>" #'jjui-visit-file)

(define-derived-mode jjui-log-mode magit-section-mode "JJ-Log"
  "Major mode for browsing `jj log' output."
  :group 'jjui
  (setq-local revert-buffer-function #'jjui--revert-buffer)
  (setq-local truncate-lines t))

(define-minor-mode jjui-abandon-mode
  "Minor mode for staging `jj abandon' selections in a `jjui' log buffer."
  :lighter " Abandon"
  :keymap jjui-abandon-mode-map
  (unless (derived-mode-p 'jjui-log-mode)
    (setq jjui-abandon-mode nil)
    (user-error "Not in a jjui log buffer"))
  (unless jjui-abandon-mode
    (setq jjui--abandon-selected-ids nil)
    (jjui--clear-abandon-overlays))
  (force-mode-line-update))

(keymap-set jjui-log-mode-map "n" #'jjui-section-forward)
(keymap-set jjui-log-mode-map "p" #'jjui-section-backward)
(keymap-set jjui-log-mode-map "^" #'jjui-section-up)
(keymap-set jjui-log-mode-map "N" #'jjui-new)
(keymap-set jjui-log-mode-map "e" #'jjui-edit)
(keymap-set jjui-log-mode-map "a" #'jjui-abandon-start)
(keymap-set jjui-log-mode-map "<wheel-up>" #'jjui-mwheel-scroll)
(keymap-set jjui-log-mode-map "<wheel-down>" #'jjui-mwheel-scroll)
(keymap-set jjui-log-mode-map "<mouse-4>" #'jjui-mwheel-scroll)
(keymap-set jjui-log-mode-map "<mouse-5>" #'jjui-mwheel-scroll)
(keymap-set jjui-log-mode-map "g" #'jjui-log-refresh)

(defun jjui-space ()
  "In abandon mode toggle the revision at point, otherwise scroll up.
This is bound on section keymaps so it wins over inherited Magit
section bindings that would otherwise page the buffer."
  (interactive)
  (if jjui-abandon-mode
      (jjui-abandon-toggle-revision)
    (call-interactively #'scroll-up-command)))

(defun jjui ()
  "Open a Jujutsu log buffer for the current repository."
  (interactive)
  (let ((buffer (get-buffer-create "*jj-log*"))
        (source-dir default-directory))
    (with-current-buffer buffer
      (jjui-log-mode)
      (setq default-directory source-dir)
      (setq jjui--repo-root (jjui--locate-root source-dir))
      (jjui-log-refresh))
    (pop-to-buffer buffer)))

(defun jjui-log-refresh ()
  "Refresh the current Jujutsu log buffer."
  (interactive)
  (unless (derived-mode-p 'jjui-log-mode)
    (user-error "Not in a jjui log buffer"))
    (let ((current-change-id (jjui--current-change-id))
          (expanded-change-ids (jjui--expanded-change-ids))
          (expanded-file-keys (jjui--expanded-file-keys)))
      (jjui--log-refresh-sync current-change-id expanded-change-ids expanded-file-keys)))

(defun jjui-new ()
  "Create a new child of the current revision and move to `@'."
  (interactive)
  (when jjui-abandon-mode
    (jjui-abandon-disabled-command))
  (let ((commit-id (jjui--require-current-commit-id)))
    (jjui--run-mutation
     (lambda ()
       (jjui--call-jj jjui--repo-root "new" commit-id))
     :target #'jjui--working-copy-commit-id)))

(defun jjui-edit ()
  "Edit the current revision and move to `@'."
  (interactive)
  (when jjui-abandon-mode
    (jjui-abandon-disabled-command))
  (let ((commit-id (jjui--require-current-commit-id)))
    (jjui--run-mutation
     (lambda ()
       (jjui--call-jj jjui--repo-root "edit" "-r" commit-id))
     :target #'jjui--working-copy-commit-id)))

(defun jjui-abandon-start ()
  "Enter abandon mode with no revisions selected yet."
  (interactive)
  (when jjui-abandon-mode
    (user-error "Already in abandon mode"))
  (setq jjui--abandon-selected-ids nil)
  (jjui-abandon-mode 1)
  (message "Abandon mode: SPC toggle, RET apply, C-g cancel"))

(defun jjui-abandon-toggle-revision ()
  "Toggle direct abandon selection for the current revision."
  (interactive)
  (jjui--abandon-toggle-current))

(defun jjui-abandon-apply ()
  "Apply the current abandon selection."
  (interactive)
  (unless jjui-abandon-mode
    (user-error "Not in abandon mode"))
  (let* ((selected-ids jjui--abandon-selected-ids)
         (fallbacks (jjui--selection-fallbacks (jjui--current-change-id))))
    (when (null selected-ids)
      (user-error "No revisions selected for abandon"))
    (jjui--run-mutation
     (lambda ()
       (apply #'jjui--call-jj jjui--repo-root
              (append (list "abandon" "--retain-bookmarks")
                      (jjui--prefixed-rev-args selected-ids))))
     :after-success (lambda ()
                      (jjui-abandon-mode -1))
     :target (lambda ()
               (or (seq-find #'jjui--revision-exists-p fallbacks)
                   (jjui--working-copy-commit-id))))))

(defun jjui-abandon-cancel ()
  "Cancel abandon mode without mutating the repository."
  (interactive)
  (unless jjui-abandon-mode
    (user-error "Not in abandon mode"))
  (jjui-abandon-mode -1)
  (message "Abandon canceled"))

(defun jjui-abandon-disabled-command ()
  "Reject commands that are unavailable while staging abandon selections."
  (interactive)
  (user-error "Disabled in abandon mode"))

(defun jjui-section-forward ()
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
      (if-let* ((next (jjui--next-section-from-point)))
          (magit-section-goto next)
        (user-error "No next section")))
     ((not (jjui--auxiliary-top-level-section-p section))
      (jjui--section-forward-skip-auxiliary))
     (t
      (magit-section-forward)))))

(defun jjui-section-backward ()
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
      (if-let* ((prev (jjui--previous-section-from-point)))
          (magit-section-goto prev)
        (user-error "No previous section")))
     ((not (jjui--auxiliary-top-level-section-p section))
      (jjui--section-backward-skip-auxiliary))
     (t
      (magit-section-backward)))))

(defun jjui-section-up ()
  "Move to the parent section.
From a file row, skip the auxiliary summary section and jump to the revision
heading.  From hunks, move to the file section."
  (interactive)
  (let* ((section (or (magit-section-at)
                      (user-error "No section at point")))
         (parent (oref section parent)))
    (cond
     ((and (object-of-class-p section 'jjui-file-section)
           (object-of-class-p parent 'jjui-summary-section)
           (oref parent parent))
      (magit-section-goto (oref parent parent)))
     (parent
      (magit-section-goto parent))
     (t
      (user-error "No parent section")))))

(defun jjui-mwheel-scroll (event &optional arg)
  "Scroll with EVENT, then keep point off the edge in `jjui-log-mode'.
Use normal `mwheel-scroll' for the viewport, but if point lands on the first or
last visible line in the target `jjui' window, move it one line inward.  This
preserves normal non-conservative recentering for keyboard motion while
preventing redisplay from doing a larger recenter before the next wheel event."
  (interactive "e\nP")
  (let ((window (ignore-errors (posn-window (event-start event))))
        (direction (event-basic-type event)))
    (mwheel-scroll event arg)
    (when (and (window-live-p window)
               (buffer-live-p (window-buffer window)))
      (with-selected-window window
        (when (derived-mode-p 'jjui-log-mode)
          (jjui--nudge-point-from-wheel-edge direction))))))

(defun jjui--nudge-point-from-wheel-edge (direction)
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

(defun jjui-toggle-at-point ()
  "Toggle the section at point.
On revisions and summaries, toggle the changed-file list.  On files and hunks,
toggle that section's body."
  (interactive)
  (let ((section (magit-section-at)))
    (cond
     ((null section)
      (user-error "No section at point"))
     ((or (object-of-class-p section 'jjui-file-section)
          (object-of-class-p section 'jjui-hunk-section))
      (when (and (object-of-class-p section 'jjui-hunk-section)
                 (oref section hidden))
        (jjui--prepare-missing-hunk-body section))
      (magit-section-toggle section)
      (when-let* ((revision (and jjui-abandon-mode (jjui--current-revision-section)))
                  (commit-id (oref revision value)))
        (jjui--restyle-abandon-revision revision commit-id)))
     (t
      (let* ((revision (jjui--current-revision-section))
             (summary (jjui--summary-child revision)))
        (unless summary
          (user-error "No section to toggle at point"))
        (magit-section-toggle summary)
        (when jjui-abandon-mode
          (jjui--restyle-abandon-revision revision (oref revision value)))
        (magit-section-goto revision))))))

(defun jjui-visit-file ()
  "Visit the file or hunk location at point.
For file rows, visit the file.  For hunk headers and body lines, visit the
corresponding line in the new side, except removed lines open the old-side
snapshot at the deleted line.

For new-side visits in `@', or its unique parent when `@' is empty, visit the
working-tree file.  For older revisions, open a read-only snapshot buffer."
  (interactive)
  (let* ((file-section (jjui--current-file-section))
         (hunk-section (jjui--current-hunk-section)))
    (unless file-section
      (user-error "No file at point"))
    (let* ((commit-id (oref file-section commit-id))
           (path (oref file-section path)))
      (if hunk-section
          (pcase-let* ((`(,side ,line ,column) (jjui--hunk-location hunk-section)))
            (if (eq side 'old)
                (jjui--visit-file-snapshot
                 (or (jjui--single-parent-commit-id commit-id)
                     (user-error "Old-side navigation is only supported for single-parent revisions"))
                 path line column)
              (jjui--visit-file-new-side commit-id path line column)))
        (jjui--visit-file-new-side commit-id path nil)))))

(defun jjui--revert-buffer (&rest _ignore)
  "Refresh the current buffer for `revert-buffer'."
  (jjui-log-refresh))

(defun jjui--locate-root (dir)
  "Return the Jujutsu repository root for DIR, or nil if none."
  (condition-case nil
      (string-trim
       (jjui--call-jj dir "root"))
    (error
     (let ((default-workspace (expand-file-name "default" dir)))
       (when (file-directory-p (expand-file-name ".jj" default-workspace))
         default-workspace)))))

(defun jjui--read-log-records ()
  "Return parsed revision records for the current buffer's repository."
  (unless jjui--repo-root
    (error "Not inside a Jujutsu repository"))
  (jjui--parse-log-output (apply #'jjui--call-jj jjui--repo-root (jjui--log-args))))

(defun jjui--log-refresh-sync (current-change-id expanded-change-ids expanded-file-keys)
  "Synchronously refresh, preserving CURRENT-CHANGE-ID and expansion state."
  (let ((inhibit-read-only t))
    (setq jjui--last-error nil)
    (erase-buffer)
    (condition-case err
        (jjui--render-records (jjui--read-log-records) current-change-id
                               expanded-change-ids expanded-file-keys)
      (error
       (jjui--render-error (error-message-string err))))))

(defun jjui--render-records (records current-change-id expanded-change-ids expanded-file-keys)
  "Render RECORDS and restore CURRENT-CHANGE-ID and EXPANDED-CHANGE-IDS."
  (setq jjui--records records)
  (if records
      (progn
        (jjui--insert-records records expanded-change-ids expanded-file-keys)
        (jjui--goto-change current-change-id)
        (unless (jjui--current-revision-section)
          (goto-char (point-min))
          (when (magit-section-at)
            (magit-section-goto (magit-section-at))))
        (jjui--sync-abandon-overlays))
    (jjui--insert-message "No revisions to show.")))

(defun jjui--render-error (message)
  "Render error MESSAGE in the current buffer."
  (setq jjui--records nil)
  (setq jjui--last-error message)
  (jjui--insert-message message))

(defun jjui--log-args ()
  "Build arguments for the `jj log' process."
  (append
   (list "log" "--color" "always" "--no-pager" "--ignore-working-copy"
         "--template" (jjui--combined-template))
   (when jjui-log-revset
     (list "--revisions" jjui-log-revset))
   (when jjui-log-limit
     (list "--limit" (number-to-string jjui-log-limit)))))

(defun jjui--combined-template ()
  "Build the Jujutsu template used to render one revision record."
  (concat "\"" jjui--record-separator "\" ++ change_id ++ \""
          jjui--record-separator "\" ++ commit_id ++ \""
          jjui--record-separator "\" ++ builtin_log_comfortable"))

(defun jjui--call-jj (dir &rest args)
  "Run `jjui-program' in DIR with ARGS and return stdout.
Signal an error if the process exits non-zero or the executable is missing."
  (unless (executable-find jjui-program)
    (error "Cannot find `%s' in PATH" jjui-program))
  (with-temp-buffer
    (let ((default-directory dir))
      (let ((exit-code (apply #'process-file jjui-program nil (current-buffer) nil args)))
        (unless (zerop exit-code)
          (error "%s" (string-trim (buffer-string))))
        (buffer-string)))))

(defun jjui--run-mutation (thunk &rest plist)
  "Run mutating THUNK with refresh and simple in-flight protection.
Keyword args:
:target is a commit id or a function returning one after success.
:after-success is a function called before refreshing after success."
  (when jjui--mutation-in-progress
    (user-error "Another jj mutation is already running"))
  (unless jjui--repo-root
    (user-error "Not inside a Jujutsu repository"))
  (let* ((jjui--mutation-in-progress t)
         (target-spec (plist-get plist :target))
         (after-success (plist-get plist :after-success))
         (current-change-id (jjui--current-change-id))
         (expanded-change-ids (jjui--expanded-change-ids))
         (expanded-file-keys (jjui--expanded-file-keys))
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
              (jjui--log-refresh-sync target expanded-change-ids expanded-file-keys))
          (error
           (message "%s" (error-message-string err))
           nil))
      (setq jjui--mutation-in-progress nil))))

(defun jjui--require-current-commit-id ()
  "Return the current revision commit id, or signal a user error."
  (or (jjui--current-change-id)
      (user-error "No revision selected")))

(defun jjui--working-copy-commit-id ()
  "Return the commit id for `@' in the current repository view."
  (string-trim
   (jjui--call-jj jjui--repo-root "log" "-r" "@"
                   "--ignore-working-copy" "--no-graph"
                   "--color" "never" "--template" "commit_id")))

(defun jjui--revision-exists-p (commit-id)
  "Return non-nil if COMMIT-ID exists in the current repo view."
  (when commit-id
    (condition-case nil
        (not (string-empty-p
              (string-trim
               (jjui--call-jj jjui--repo-root "log" "-r" commit-id
                               "--ignore-working-copy" "--no-graph"
                               "--color" "never" "--template" "commit_id"))))
      (error nil))))

(defun jjui--selection-fallbacks (commit-id)
  "Return candidate revisions to select if COMMIT-ID vanishes after mutation."
  (let ((ids (jjui--revision-commit-ids)))
    (when-let* ((pos (cl-position commit-id ids :test #'equal)))
      (delq nil (list (nth (1+ pos) ids)
                      (nth (1- pos) ids)
                      (jjui--working-copy-commit-id))))))

(defun jjui--revision-commit-ids ()
  "Return commit ids for visible revision sections in buffer order."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (object-of-class-p child 'jjui-revision-section)
          (push (oref child value) ids))))
    (nreverse ids)))

(defun jjui--prefixed-rev-args (commit-ids)
  "Return `-r' args for COMMIT-IDS."
  (cl-mapcan (lambda (commit-id) (list "-r" commit-id)) commit-ids))

(defun jjui--abandon-toggle-current ()
  "Toggle current revision in the abandon selection."
  (unless jjui-abandon-mode
    (user-error "Not in abandon mode"))
  (let ((commit-id (jjui--require-current-commit-id))
        (revision (jjui--current-revision-section)))
    (setq jjui--abandon-selected-ids
          (if (member commit-id jjui--abandon-selected-ids)
              (cl-remove commit-id jjui--abandon-selected-ids :test #'equal)
            (cons commit-id jjui--abandon-selected-ids)))
    (jjui--restyle-abandon-revision revision commit-id)))

(defun jjui--refresh-abandon-display ()
  "Refresh abandon overlays in the current buffer."
  (jjui--sync-abandon-overlays))

(defun jjui--restyle-abandon-revision (revision commit-id)
  "Update abandon styling overlay for REVISION in place."
  (when revision
    (let ((inhibit-read-only t))
      (jjui--delete-abandon-overlay commit-id)
      (when (member commit-id jjui--abandon-selected-ids)
        (let ((overlay (make-overlay (oref revision start) (oref revision end) nil t t)))
          (overlay-put overlay 'jjui-abandon-commit-id commit-id)
          (overlay-put overlay 'face 'jjui-abandon-included-row)
          (push overlay jjui--abandon-overlays))))))

(defun jjui--sync-abandon-overlays ()
  "Rebuild abandon overlays for all selected revisions in the current buffer."
  (when jjui-abandon-mode
    (jjui--clear-abandon-overlays)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when (and (object-of-class-p revision 'jjui-revision-section)
                   (member (oref revision value) jjui--abandon-selected-ids))
          (jjui--restyle-abandon-revision revision (oref revision value)))))))

(defun jjui--delete-abandon-overlay (commit-id)
  "Delete the abandon overlay for COMMIT-ID, if present."
  (setq jjui--abandon-overlays
        (cl-remove-if (lambda (overlay)
                        (when (equal (overlay-get overlay 'jjui-abandon-commit-id) commit-id)
                          (delete-overlay overlay)
                          t))
                      jjui--abandon-overlays)))

(defun jjui--clear-abandon-overlays ()
  "Remove all abandon styling overlays from the current buffer."
  (mapc #'delete-overlay jjui--abandon-overlays)
  (setq jjui--abandon-overlays nil))

(defun jjui--revision-heading-text (record)
  "Return propertized heading text for revision RECORD."
  (let* ((commit-id (plist-get record :commit-id))
         (heading (jjui--ansi-colorize (plist-get record :heading))))
    (ignore commit-id)
    heading))

(defun jjui--revision-summary-text (record)
  "Return propertized summary text for revision RECORD."
  (jjui--ansi-colorize (plist-get record :summary)))

(defun jjui--body-text (string commit-id)
  "Return propertized body STRING for COMMIT-ID, preserving color."
  (ignore commit-id)
  (jjui--ansi-colorize string))

(defun jjui--parse-log-output (output)
  "Parse `jj log --summary' OUTPUT into revision records."
  (let ((lines (split-string output "\n"))
        records
        current
        expecting-summary)
    (dolist (line lines)
      (cond
       ((jjui--parse-heading-line line)
        (when current
          (push (nreverse current) records))
        (setq current (list (jjui--parse-heading-line line)))
        (setq expecting-summary t))
       ((jjui--elided-line-p line)
        (when current
          (push (nreverse current) records))
        (push (list (cons 'elided line)) records)
        (setq current nil)
        (setq expecting-summary nil))
       ((and (not expecting-summary) (jjui--graph-continuation-line-p line))
        (when current
          (push (nreverse current) records))
        (push (list (cons 'graph line)) records)
        (setq current nil)
        (setq expecting-summary nil))
       ((and current expecting-summary)
        (push (cons 'summary (jjui--normalize-summary-line line)) current)
        (setq expecting-summary nil))
       ((and current (not (string-empty-p line)))
        ;; Ignore any extra lines from the visible log template. File bodies are
        ;; loaded lazily on expansion instead of being pre-rendered into the
        ;; buffer.
        nil)))
    (when current
      (push (nreverse current) records))
    (nreverse (mapcar #'jjui--record-alist-to-plist records))))

(defun jjui--parse-heading-line (line)
  "If LINE starts a revision record, return an alist entry for it."
  (let ((regexp (concat "^\\(.*?\\)" (regexp-quote jjui--record-separator)
                        "\\([^" (regexp-quote jjui--record-separator) "]+\\)"
                        (regexp-quote jjui--record-separator)
                        "\\([^" (regexp-quote jjui--record-separator) "]+\\)"
                        (regexp-quote jjui--record-separator)
                        "\\(.*\\)$")))
    (when (string-match regexp line)
      (let ((prefix (match-string 1 line))
            (change-id (match-string 2 line))
            (commit-id (match-string 3 line))
            (text (match-string 4 line)))
        (cons 'heading
              (list :change-id (jjui--strip-ansi change-id)
                    :commit-id (jjui--strip-ansi commit-id)
                    :text (concat prefix text)))))))

(defun jjui--record-alist-to-plist (record)
  "Convert parsed RECORD alist into a plist."
  (if-let* ((elided (alist-get 'elided record)))
      (list :kind 'elided :heading elided)
    (if-let* ((graph (alist-get 'graph record)))
        (list :kind 'graph :heading graph)
    (let* ((heading-entry (alist-get 'heading record))
           (summary (or (alist-get 'summary record) "")))
      (list :kind 'revision
            :change-id (plist-get heading-entry :change-id)
            :commit-id (plist-get heading-entry :commit-id)
            :heading (plist-get heading-entry :text)
            :summary summary)))))

(defun jjui--insert-records (records expanded-change-ids expanded-file-keys)
  "Insert RECORDS, restoring expanded revisions and files from saved state."
  (magit-insert-section (jjui-root-section)
    (dolist (record records)
      (pcase (plist-get record :kind)
        ('elided
         (magit-insert-section (jjui-elided-section)
           (magit-insert-heading (jjui--ansi-colorize (plist-get record :heading)))))
        ('graph
         (magit-insert-section (jjui-graph-line-section)
           (magit-insert-heading (jjui--ansi-colorize (plist-get record :heading)))))
        (_
         (let* ((commit-id (plist-get record :commit-id))
               (expanded (member commit-id expanded-change-ids)))
           (magit-insert-section (jjui-revision-section commit-id)
             (magit-insert-heading (jjui--revision-heading-text record))
             (magit-insert-section (jjui-summary-section commit-id (not expanded))
               (magit-insert-heading (jjui--revision-summary-text record))
               (magit-insert-section-body
                 (jjui--insert-file-summary commit-id expanded-file-keys))))))))))

(defun jjui--insert-message (message)
  "Insert MESSAGE as plain buffer contents."
  (insert message)
  (unless (bolp)
    (insert "\n"))
  (goto-char (point-min)))

(defun jjui--current-revision-section ()
  "Return the revision section at point, or its revision ancestor."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'jjui-revision-section)))
      (setq section (oref section parent)))
    section))

(defun jjui--current-file-section ()
  "Return the file section at point, or its file ancestor."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'jjui-file-section)))
      (setq section (oref section parent)))
    section))

(defun jjui--current-hunk-section ()
  "Return the hunk section at point, or nil."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section 'jjui-hunk-section)))
      (setq section (oref section parent)))
    section))

(defun jjui--current-change-id ()
  "Return the current revision's commit id, or nil."
  (when-let* ((section (jjui--current-revision-section)))
    (oref section value)))

(defun jjui--summary-child (revision)
  "Return the summary child section for REVISION."
  (when revision
    (seq-find (lambda (child)
                (object-of-class-p child 'jjui-summary-section))
              (oref revision children))))

(defun jjui--next-section-from-point ()
  "Return the next section at or after point, or nil."
  (let ((pos (point))
        section)
    (while (and (< pos (point-max))
                (not (setq section (magit-section-at pos))))
      (setq pos (or (next-single-property-change pos 'magit-section nil (point-max))
                    (point-max))))
    section))

(defun jjui--previous-section-from-point ()
  "Return the previous section at or before point, or nil."
  (let ((pos (min (point) (1- (point-max))))
        section)
    (while (and (> pos (point-min))
                (not (setq section (magit-section-at pos))))
      (setq pos (or (previous-single-property-change pos 'magit-section nil (point-min))
                    (point-min))))
    (or section (magit-section-at pos))))

(defun jjui--auxiliary-top-level-section-p (section)
  "Return non-nil if SECTION is a top-level row to skip from revision headings."
  (or (object-of-class-p section 'jjui-summary-section)
      (object-of-class-p section 'jjui-elided-section)
      (object-of-class-p section 'jjui-graph-line-section)))

(defun jjui--section-forward-skip-auxiliary ()
  "Move forward, skipping auxiliary rows unless already on one."
  (let ((start (point)))
    (condition-case err
        (progn
          (magit-section-forward)
          (while (jjui--auxiliary-top-level-section-p (magit-section-at))
            (magit-section-forward)))
      (user-error
       (goto-char start)
       (signal (car err) (cdr err))))))

(defun jjui--section-backward-skip-auxiliary ()
  "Move backward, skipping auxiliary rows unless already on one."
  (let ((start (point)))
    (condition-case err
        (progn
          (magit-section-backward)
          (while (jjui--auxiliary-top-level-section-p (magit-section-at))
            (magit-section-backward)))
      (user-error
       (goto-char start)
       (signal (car err) (cdr err))))))

(defun jjui--expanded-change-ids ()
  "Return commit ids whose file sections are currently expanded."
  (let (ids)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when-let* ((summary (jjui--summary-child revision)))
          (when (not (oref summary hidden))
            (push (oref revision value) ids)))))
    ids))

(defun jjui--expanded-file-keys ()
  "Return (COMMIT-ID . PATH) pairs for currently expanded file sections."
  (let (keys)
    (when (bound-and-true-p magit-root-section)
      (dolist (revision (oref magit-root-section children))
        (when-let* ((summary (jjui--summary-child revision)))
          (when (not (oref summary hidden))
            (dolist (child (oref summary children))
              (when (and (object-of-class-p child 'jjui-file-section)
                         (not (oref child hidden)))
                (push (cons (oref child commit-id) (oref child path)) keys)))))))
    keys))

(defun jjui--goto-change (commit-id)
  "Move point to COMMIT-ID if present in the current buffer."
  (when (and commit-id (bound-and-true-p magit-root-section))
    (when-let* ((section (seq-find (lambda (child)
                                     (and (object-of-class-p child 'jjui-revision-section)
                                          (equal (oref child value) commit-id)))
                                   (oref magit-root-section children))))
      (magit-section-goto section))))

(defun jjui--elided-line-p (line)
  "Return non-nil if LINE is Jujutsu's synthetic elided-revisions row."
  (string-match-p "(elided revisions)" line))

(defun jjui--graph-continuation-line-p (line)
  "Return non-nil if LINE is a graph-only junction row from `jj log'.
Preserve merge/elbow rows like \"│ ├─╯\", but drop plain vertical spacer rows
like \"│\" or \"│ │\" to avoid adding extra blank space between revisions."
  (let ((plain (string-trim (jjui--strip-ansi line))))
    (and (not (string-empty-p plain))
         (not (string-match-p "[[:alnum:]]" plain))
         (string-match-p "[├└┼┤┬╯╮╭╰─]" plain))))

(defun jjui--ansi-colorize (string)
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

(defun jjui--strip-ansi (string)
  "Return STRING with ANSI escape sequences removed."
  (with-temp-buffer
    (insert string)
    (ansi-color-filter-region (point-min) (point-max))
    (buffer-string)))

(defun jjui--insert-file-summary (commit-id expanded-file-keys)
  "Insert a lazily loaded file summary for COMMIT-ID.
Restore expanded file diffs listed in EXPANDED-FILE-KEYS."
  (let* ((prefix (jjui--file-summary-prefix))
         (output (string-trim-right
                  (jjui--call-jj jjui--repo-root "diff" "--summary"
                                  "--color" "always" "-r" commit-id))))
    (if (string-empty-p output)
        (insert (jjui--body-text (concat prefix "(no files changed)") commit-id) "\n")
      (dolist (line (split-string output "\n"))
        (let* ((path (jjui--summary-line-path line))
               (expanded (member (cons commit-id path) expanded-file-keys)))
          (magit-insert-section (jjui-file-section (cons commit-id path) (not expanded)
                                                    :commit-id commit-id
                                                    :path path)
            (magit-insert-heading (jjui--body-text (concat prefix line) commit-id))
            (magit-insert-section-body
              (jjui--insert-file-diff commit-id path))))))))

(defun jjui--insert-file-diff (commit-id path)
  "Insert hunk sections for PATH in COMMIT-ID."
  (let* ((prefix (jjui--file-summary-prefix))
         (output (string-trim-right
                  (jjui--call-jj jjui--repo-root "diff" "--git"
                                  "--color" "always" "-r" commit-id "--" path)))
         (lines (if (string-empty-p output) nil (split-string output "\n")))
         (preamble nil)
         (hunk-header nil)
         (hunk-body nil)
         (saw-hunk nil)
         (inserted-preamble nil)
         (hunk-index 0))
    (cl-labels ((flush-hunk ()
                           (when hunk-header
                             (let ((body-lines (nreverse hunk-body)))
                               (pcase-let* ((`(,old-start ,old-count ,new-start ,new-count)
                                            (or (jjui--parse-hunk-header hunk-header)
                                                (list nil nil nil nil))))
                                 (unless inserted-preamble
                                   (dolist (line (nreverse preamble))
                                     (insert (jjui--body-text
                                              (concat prefix line) commit-id) "\n"))
                                   (setq inserted-preamble t))
                                 (setq saw-hunk t)
                                 ;; Give each hunk a unique value so Magit can
                                 ;; track nested visibility independently.
                                 (magit-insert-section (jjui-hunk-section hunk-index nil
                                                                           :old-start old-start
                                                                           :old-count old-count
                                                                           :new-start new-start
                                                                           :new-count new-count
                                                                           :body-prefix prefix
                                                                           :body-lines body-lines)
                                   (magit-insert-heading
                                    (jjui--body-text (concat prefix hunk-header) commit-id))
                                   (magit-insert-section-body
                                     (jjui--insert-hunk-body-lines prefix body-lines commit-id)))
                                 (setq hunk-index (1+ hunk-index))
                                 (setq hunk-header nil)
                                 (setq hunk-body nil))))))
      (dolist (line lines)
        (if (jjui--hunk-header-line-p line)
            (progn
              (flush-hunk)
              (setq hunk-header line))
          (if hunk-header
              (push line hunk-body)
            (push line preamble))))
      (flush-hunk)
      (unless inserted-preamble
        (dolist (line (nreverse preamble))
          (insert (jjui--body-text (concat prefix line) commit-id) "\n")))
      (unless (or lines saw-hunk)
        (insert (jjui--body-text (concat prefix "(no diff)") commit-id) "\n")))))

(defun jjui--insert-hunk-body-lines (prefix body-lines commit-id)
  "Insert BODY-LINES for a hunk using PREFIX and preserve color.
Apply abandon styling for COMMIT-ID when relevant."
  (dolist (body-line body-lines)
    (insert (jjui--body-text (concat prefix body-line) commit-id) "\n")))

(defun jjui--prepare-missing-hunk-body (hunk)
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
                       (and (object-of-class-p file 'jjui-file-section)
                            (oref file commit-id)))))
      (oset hunk washer
            (lambda ()
              (jjui--insert-hunk-body-lines prefix body-lines commit-id))))))

(defun jjui--file-summary-prefix ()
  "Return the graph gutter prefix to use for lazily loaded file rows."
  (let* ((summary magit-insert-section--current)
         (heading (if summary
                      (buffer-substring-no-properties (oref summary start)
                                                      (oref summary content))
                    "")))
    (if (string-match "\\`\\([^[:alnum:]]*\\)" heading)
        (jjui--continuation-prefix (match-string 1 heading))
      "")))

(defun jjui--continuation-prefix (prefix)
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

(defun jjui--normalize-summary-line (line)
  "Ensure blank summary LINE still shows a placeholder description."
  (if (string-match-p "[[:alnum:]]" line)
      line
    (concat line "(no description set)")))

(defun jjui--summary-line-path (line)
  "Extract a path from a `jj diff --summary' LINE."
  (let ((plain (jjui--strip-ansi line)))
    (if (string-match "\\`[^[:space:]]+[[:space:]]+\\(.+\\)\\'" plain)
        (jjui--normalize-summary-path (match-string 1 plain))
      plain)))

(defun jjui--normalize-summary-path (path)
  "Return the likely working-tree path from summary PATH text."
  (if (string-match " -> \\(.+\\)\\'" path)
      (match-string 1 path)
    path))

(defun jjui--hunk-header-line-p (line)
  "Return non-nil if LINE is a unified diff hunk header."
  (string-prefix-p "@@ " (jjui--strip-ansi line)))

(defun jjui--parse-hunk-header (line)
  "Parse unified diff hunk header LINE.
Return a list (OLD-START OLD-COUNT NEW-START NEW-COUNT), or nil if LINE does not
look like a hunk header."
  (let ((plain (jjui--strip-ansi line)))
    (when (string-match
           "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
           plain)
      (list (string-to-number (match-string 1 plain))
            (string-to-number (or (match-string 2 plain) "1"))
            (string-to-number (match-string 3 plain))
            (string-to-number (or (match-string 4 plain) "1"))))))

(defun jjui--hunk-location (hunk)
  "Return (SIDE LINE COLUMN) for HUNK at point.
SIDE is `new' for added/context lines and `old' for removed lines.
When point is on the hunk header, use the first changed line in the body."
  (save-excursion
    (when (< (point) (oref hunk content))
      (goto-char (oref hunk content))
      (unless (jjui--advance-to-first-changed-hunk-line hunk)
        (user-error "No changed line in hunk")))
    (jjui--hunk-location-at-point hunk)))

(defun jjui--advance-to-first-changed-hunk-line (hunk)
  "Move point to the first added or removed line in HUNK.
Return non-nil if one was found."
  (let ((end (oref hunk end))
        found)
    (while (and (not found) (< (point) end))
      (let ((kind (jjui--current-hunk-line-kind hunk)))
        (if (memq kind '(old new))
            (setq found t)
          (forward-line 1))))
    found))

(defun jjui--hunk-location-at-point (hunk)
  "Compute the diff side, line number, and source column for point inside HUNK."
  (let ((old (or (oref hunk old-start)
                 (user-error "Cannot parse hunk header")))
        (new (or (oref hunk new-start)
                 (user-error "Cannot parse hunk header")))
        (target-line (line-beginning-position))
        ;; Capture the user's horizontal position before walking from the hunk
        ;; start to count old/new line numbers.  The scan below leaves point at
        ;; BOL, so computing the column afterward would always return 0.
        (source-column (jjui--current-hunk-source-column hunk))
        side
        line)
    (save-excursion
      (goto-char (oref hunk content))
      (while (and (< (point) target-line)
                  (< (point) (oref hunk end)))
        (pcase (jjui--current-hunk-line-kind hunk)
          ('old (setq old (1+ old)))
          ('new (setq new (1+ new)))
          ('context (setq old (1+ old)
                          new (1+ new)))
          (_ nil))
        (forward-line 1))
      (pcase (jjui--current-hunk-line-kind hunk)
        ('old (setq side 'old
                    line old))
        ('new (setq side 'new
                    line new))
        ('context (setq side 'new
                        line new))
        (_ (setq side 'new
                 line new))))
    (list side line source-column)))

(defun jjui--current-hunk-source-column (hunk)
  "Return the source column corresponding to point on the current hunk line.
If point is still in the graph gutter or diff marker, use column 0."
  (let* ((body-prefix (or (oref hunk body-prefix) ""))
         (marker-column (+ (length body-prefix) 1))
         (offset (- (current-column) marker-column)))
    (max 0 offset)))

(defun jjui--current-hunk-line-kind (hunk)
  "Return the kind of the current line in HUNK.
The result is one of `old', `new', `context', `meta', or nil."
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
         (body-prefix (or (oref hunk body-prefix) ""))
         (plain (jjui--strip-ansi line))
         (diff-line (if (string-prefix-p body-prefix plain)
                        (substring plain (length body-prefix))
                      plain)))
    (pcase (and (> (length diff-line) 0) (aref diff-line 0))
      (?- 'old)
      (?+ 'new)
      (?\s 'context)
      (?\\ 'meta)
      (_ nil))))

(defun jjui--working-tree-commit-p (commit-id)
  "Return non-nil if COMMIT-ID should visit the working-tree file."
  (member commit-id (jjui--working-tree-commit-ids)))

(defun jjui--working-tree-commit-ids ()
  "Return commit ids that should open files from the working tree."
  (let* ((current (string-trim
                   (jjui--call-jj jjui--repo-root "log" "-r" "@"
                                   "--ignore-working-copy"
                                   "--no-graph" "--color" "never"
                                   "--template" "commit_id")))
         (ids (list current))
         (current-summary (string-trim
                           (jjui--call-jj jjui--repo-root "diff" "--summary"
                                           "--ignore-working-copy"
                                           "--color" "never" "-r" "@"))))
    (when (string-empty-p current-summary)
      (let* ((parents-output (string-trim
                              (jjui--call-jj jjui--repo-root "log" "-r" "parents(@)"
                                              "--ignore-working-copy"
                                              "--no-graph" "--color" "never"
                                              "--template" "commit_id ++ \"\\n\"")))
             (parents (seq-remove #'string-empty-p (split-string parents-output "\n"))))
        (when (= (length parents) 1)
          (push (car parents) ids))))
    ids))

(defun jjui--single-parent-commit-id (commit-id)
  "Return the unique parent commit id of COMMIT-ID, or nil if not unique."
  (let* ((parents-output (string-trim
                          (jjui--call-jj jjui--repo-root "log"
                                          "-r" (format "parents(%s)" commit-id)
                                          "--no-graph" "--color" "never"
                                          "--template" "commit_id ++ \"\\n\"")))
         (parents (seq-remove #'string-empty-p (split-string parents-output "\n"))))
    (when (= (length parents) 1)
      (car parents))))

(defun jjui--visit-file-new-side (commit-id path &optional line column)
  "Visit PATH from the new side of COMMIT-ID, optionally at LINE and COLUMN.
Use the working tree for `@' and its empty-parent special case, otherwise open a
read-only snapshot."
  (let ((worktree-path (expand-file-name path jjui--repo-root)))
    (if (and (jjui--working-tree-commit-p commit-id)
             (file-exists-p worktree-path))
        (progn
          (find-file worktree-path)
          (jjui--goto-line-column line column))
      (jjui--visit-file-snapshot commit-id path line column))))

(defun jjui--visit-file-snapshot (commit-id path &optional line column)
  "Open PATH from COMMIT-ID in a read-only buffer, optionally at LINE and COLUMN."
  (let* ((content (jjui--call-jj jjui--repo-root "file" "show"
                                  "--color" "never" "--no-pager"
                                  "--ignore-working-copy" "-r" commit-id "--" path))
         (short (substring commit-id 0 (min 8 (length commit-id))))
         (buffer (get-buffer-create (format "*jjui %s:%s*" short path)))
         window)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))
        (jjui--goto-line-column line column)
        (setq-local buffer-read-only t)))
    ;; Reuse an existing window for this snapshot buffer when possible and set
    ;; point after the buffer is displayed.  That avoids `pop-to-buffer'
    ;; restoring a stale window-point when reopening the same temporary buffer.
    (setq window (or (get-buffer-window buffer 0)
                     (display-buffer buffer)))
    (when (window-live-p window)
      (select-window window)
      (with-current-buffer buffer
        (jjui--goto-line-column line column)
        (set-window-point window (point))
        (set-window-start window (line-beginning-position) t)))))

(defun jjui--goto-line-column (&optional line column)
  "Move to LINE and COLUMN in the current buffer when provided."
  (when line
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (when column
      (move-to-column column))))

(provide 'jjui)

;;; jjui.el ends here
