;;; majjic-jj.el --- Jujutsu process helpers for Majjic -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andy Phan
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc

;;; Commentary:

;; Process, query, and mutation helpers for `majjic'.  These are kept separate
;; from buffer rendering so repository I/O has one obvious home.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'majjic-model)

(declare-function majjic--capture-refresh-state "majjic")
(declare-function majjic--current-commit-id "majjic-actions")
(declare-function majjic--expanded-commit-ids "majjic")
(declare-function majjic--expanded-file-keys "majjic")
(declare-function majjic--log-refresh-async "majjic")
(declare-function majjic--log-refresh-sync "majjic")
(declare-function majjic--parse-log-output "majjic-model")
(declare-function majjic--revision-commit-ids "majjic")
(declare-function majjic--set-operation-status "majjic")
(defvar majjic--record-separator)
(defvar majjic--mutation-in-progress)
(defvar majjic--mutation-process)
(defvar majjic--process-generation)
(defvar majjic--repo-root)
(defvar majjic-log-limit)
(defvar majjic-log-revset)
(defvar majjic-program)

(cl-defstruct (majjic-operation-record
               (:constructor majjic--make-operation-record))
  id
  description
  parent-ids)

(defconst majjic--undo-op-desc-prefix "undo: restore to operation "
  "Prefix used by `jj undo' operation descriptions.")

(defconst majjic--redo-op-desc-prefix "redo: restore to operation "
  "Prefix used by `jj redo' operation descriptions.")

(defun majjic--read-log-records ()
  "Return parsed revision records for the current buffer's repository."
  (unless majjic--repo-root
    (error "Not inside a Jujutsu repository"))
  (majjic--parse-log-output (apply #'majjic--call-jj majjic--repo-root (majjic--log-args))))

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
  (pcase-let ((`(,exit-code ,stdout ,stderr) (apply #'majjic--call-jj-capture dir args)))
    (if (zerop exit-code)
        stdout
      (error "%s" (majjic--jj-error-message stdout stderr)))))

(defun majjic--call-jj-capture (dir &rest args)
  "Run `majjic-program' in DIR with ARGS.
Return a list of (EXIT-CODE STDOUT STDERR)."
  (unless (executable-find majjic-program)
    (error "Cannot find `%s' in PATH" majjic-program))
  (let ((stderr-file (make-temp-file "majjic-jj-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory dir))
            (let ((exit-code (apply #'process-file majjic-program nil
                                    (list (current-buffer) stderr-file) nil args))
                  (stdout (buffer-string))
                  (stderr (with-temp-buffer
                            (insert-file-contents stderr-file)
                            (buffer-string))))
              (list exit-code stdout stderr))))
      (ignore-errors (delete-file stderr-file)))))

(defun majjic--jj-error-message (stdout stderr)
  "Return a trimmed error message from STDOUT and STDERR."
  (string-trim (if (string-blank-p stderr) stdout stderr)))

(defun majjic--call-jj-capture-async (dir callback &rest args)
  "Run `majjic-program' in DIR with ARGS and invoke CALLBACK on exit.
CALLBACK receives (EXIT-CODE STDOUT STDERR).  If the originating buffer is
killed before the process exits, drop the callback and clean up process
buffers."
  (unless (executable-find majjic-program)
    (error "Cannot find `%s' in PATH" majjic-program))
  (let* ((source-buffer (current-buffer))
         (stdout-buffer (generate-new-buffer " *majjic-jj-stdout*"))
         (stderr-buffer (generate-new-buffer " *majjic-jj-stderr*"))
         (process
          (let ((default-directory dir))
            (make-process
             :name "majjic-jj"
             :buffer stdout-buffer
             :stderr stderr-buffer
             :command (cons majjic-program args)
             :connection-type 'pipe
             :noquery t
             :sentinel #'majjic--jj-process-sentinel))))
    (process-put process 'majjic-source-buffer source-buffer)
    (process-put process 'majjic-stdout-buffer stdout-buffer)
    (process-put process 'majjic-stderr-buffer stderr-buffer)
    (process-put process 'majjic-callback callback)
    process))

(defun majjic--jj-process-sentinel (process _event)
  "Handle completion for asynchronous Jujutsu PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let* ((source-buffer (process-get process 'majjic-source-buffer))
           (stdout-buffer (process-get process 'majjic-stdout-buffer))
           (stderr-buffer (process-get process 'majjic-stderr-buffer))
           (callback (process-get process 'majjic-callback))
           (exit-code (process-exit-status process))
           (stdout (if (buffer-live-p stdout-buffer)
                       (with-current-buffer stdout-buffer
                         (buffer-string))
                     ""))
           (stderr (if (buffer-live-p stderr-buffer)
                       (with-current-buffer stderr-buffer
                         (buffer-string))
                     "")))
      (unwind-protect
          (when (and callback (buffer-live-p source-buffer))
            (run-at-time
             0 nil
             (lambda ()
               (when (buffer-live-p source-buffer)
                 (with-current-buffer source-buffer
                   (funcall callback exit-code stdout stderr))))))
        (when (buffer-live-p stdout-buffer)
          (kill-buffer stdout-buffer))
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))))))

(defun majjic--operation-process-live-p (process)
  "Return non-nil if PROCESS is a live process."
  (and (processp process)
       (process-live-p process)))

(defun majjic--next-process-generation ()
  "Increment and return the current process generation."
  (setq majjic--process-generation (1+ majjic--process-generation)))

(defun majjic--clear-mutation-state ()
  "Clear mutation tracking and mode-line state in the current buffer."
  (setq majjic--mutation-process nil)
  (setq majjic--mutation-in-progress nil)
  (majjic--set-operation-status nil))

(defun majjic--run-mutation (command &rest plist)
  "Run mutating COMMAND asynchronously with refresh and in-flight protection.
COMMAND is either an argument list or a function returning one.
Keyword args:
:target is a commit id or a function returning one after success.
:after-success is a function called before refreshing after success."
  (when majjic--mutation-in-progress
    (user-error "Another jj mutation is already running"))
  (unless majjic--repo-root
    (user-error "Not inside a Jujutsu repository"))
  (let* ((target-spec (plist-get plist :target))
         (after-success (plist-get plist :after-success))
         (args (if (functionp command) (funcall command) command))
         (generation (majjic--next-process-generation))
         process)
    (setq majjic--mutation-in-progress t)
    (majjic--set-operation-status "Running")
    (condition-case err
        (progn
          (setq process
                (apply #'majjic--call-jj-capture-async majjic--repo-root
                       (lambda (exit-code stdout stderr)
                         (when (= generation majjic--process-generation)
                           (setq majjic--mutation-process nil)
                           (if (zerop exit-code)
                               (condition-case callback-err
                                   (let* ((current-state (majjic--capture-refresh-state))
                                          (target (majjic-state-current-commit-id current-state)))
                                     (when after-success
                                       (funcall after-success))
                                     (setq target
                                           (cond
                                            ((functionp target-spec) (funcall target-spec))
                                            (target-spec target-spec)
                                            (t target)))
                                     (setf (majjic-state-current-commit-id current-state) target)
                                     (majjic--log-refresh-async current-state generation))
                                 (error
                                  (message "%s" (error-message-string callback-err))
                                  (majjic--clear-mutation-state)))
                             (message "%s" (majjic--jj-error-message stdout stderr))
                             (majjic--clear-mutation-state))))
                       args))
          (setq majjic--mutation-process process))
      (error
       (majjic--clear-mutation-state)
       (signal (car err) (cdr err))))))

(defun majjic--require-current-commit-id ()
  "Return the current revision commit id, or signal a user error."
  (or (majjic--current-commit-id)
      (user-error "No revision selected")))

(defun majjic--working-copy-commit-id ()
  "Return the commit id for `@' in the current repository view."
  (string-trim
   (majjic--call-jj majjic--repo-root "log" "-r" "@"
                   "--ignore-working-copy" "--no-graph"
                   "--color" "never" "--template" "commit_id")))

(defun majjic--change-id-for-commit-id (commit-id)
  "Return the change id for COMMIT-ID in the current repository view."
  (string-trim
   (majjic--call-jj majjic--repo-root "log" "-r" commit-id
                   "--ignore-working-copy" "--no-graph"
                   "--color" "never" "--template" "change_id")))

(defun majjic--commit-id-for-change-id (change-id)
  "Return the current commit id for CHANGE-ID, or nil if it no longer exists."
  (when change-id
    (condition-case nil
        (let ((commit-id (string-trim
                          (majjic--call-jj majjic--repo-root "log" "-r" change-id
                                          "--ignore-working-copy" "--no-graph"
                                          "--color" "never" "--template" "commit_id"))))
          (unless (string-empty-p commit-id)
            commit-id))
      (error nil))))

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

(defun majjic--prefixed-rev-args (commit-ids)
  "Return `-r' args for COMMIT-IDS."
  (cl-mapcan (lambda (commit-id) (list "-r" commit-id)) commit-ids))

(defun majjic--undo-args ()
  "Return `jj undo' arguments."
  '("undo"))

(defun majjic--redo-args ()
  "Return `jj redo' arguments."
  '("redo"))

(defun majjic--git-fetch-args (&optional tracked)
  "Return `jj git fetch' arguments.
When TRACKED is non-nil, include `--tracked'."
  (append '("git" "fetch")
          (when tracked
            '("--tracked"))))

(defun majjic--git-push-change-args (commit-ids &optional dry-run)
  "Return `jj git push --change' arguments for COMMIT-IDS.
When DRY-RUN is non-nil, include `--dry-run'."
  (append
   (list "git" "push")
   (cl-mapcan (lambda (commit-id) (list "--change" commit-id)) commit-ids)
   (when dry-run
     (list "--dry-run"))))

(defun majjic--git-push-preview (commit-ids)
  "Return colored dry-run output for pushing COMMIT-IDS by change."
  (pcase-let ((`(,exit-code ,output) (majjic--git-push-preview-result commit-ids)))
    (if (zerop exit-code)
        output
      (error "%s" output))))

(defun majjic--git-push-preview-result (commit-ids)
  "Return (EXIT-CODE OUTPUT) for a colored dry-run push of COMMIT-IDS."
  (pcase-let* ((`(,exit-code ,stdout ,stderr)
                (apply #'majjic--call-jj-capture majjic--repo-root
                       (append
                        (majjic--git-push-change-args commit-ids t)
                        (list "--color" "always" "--no-pager"))))
               (output (string-trim-right
                        (if (string-blank-p stdout) stderr stdout))))
    (list exit-code output)))

(defun majjic--git-push-preview-result-async (commit-ids callback)
  "Run a colored dry-run push for COMMIT-IDS and invoke CALLBACK on exit.
CALLBACK receives (EXIT-CODE OUTPUT)."
  (apply #'majjic--call-jj-capture-async majjic--repo-root
         (lambda (exit-code stdout stderr)
           (funcall callback
                    exit-code
                    (string-trim-right
                     (if (string-blank-p stdout) stderr stdout))))
         (append
          (majjic--git-push-change-args commit-ids t)
          (list "--color" "always" "--no-pager"))))

(defun majjic--op-log-preview (&optional limit)
  "Return the latest operation log text for confirmation preview.
LIMIT defaults to 1."
  (majjic--operation-log-entry "@" t limit))

(defun majjic--operation-log-entry (&optional operation-id color limit)
  "Return `jj op log' text for OPERATION-ID.
OPERATION-ID defaults to `@'.  When COLOR is non-nil, preserve ANSI colors.
LIMIT defaults to 1."
  (string-trim-right
   (apply #'majjic--call-jj majjic--repo-root
          (append
           (list "--at-op" (or operation-id "@")
                 "--ignore-working-copy"
                 "op" "log" "-n" (number-to-string (or limit 1))
                 "--color" (if color "always" "never")
                 "--no-pager")))))

(defun majjic--read-operation-record (&optional operation-id)
  "Return metadata for OPERATION-ID, defaulting to `@'."
  (let* ((output
          (string-trim-right
           (apply #'majjic--call-jj majjic--repo-root
                  (append
                   (list "--at-op" (or operation-id "@")
                         "--ignore-working-copy"
                         "op" "log" "-n" "1" "--no-graph"
                         "--color" "never" "--no-pager"
                         "--template"
                         "separate(\"\\x1f\", id, description, parents.map(|o| o.id()).join(\",\")) ++ \"\\n\"")))))
         (fields (split-string output "\x1f")))
    (majjic--make-operation-record
     :id (nth 0 fields)
     :description (or (nth 1 fields) "")
     :parent-ids (seq-remove #'string-empty-p
                             (split-string (or (nth 2 fields) "") "," t)))))

(defun majjic--operation-restored-id (operation-record prefix)
  "Return the operation id restored by OPERATION-RECORD with PREFIX."
  (when-let* ((id (string-remove-prefix prefix
                                        (majjic-operation-record-description operation-record))))
    (unless (equal id (majjic-operation-record-description operation-record))
      id)))

(defun majjic--synthetic-operation-restored-id (operation-record)
  "Return the restored operation id if OPERATION-RECORD is synthetic."
  (or (majjic--operation-restored-id operation-record majjic--undo-op-desc-prefix)
      (majjic--operation-restored-id operation-record majjic--redo-op-desc-prefix)))

(defun majjic--terminal-non-synthetic-operation-record (operation-record)
  "Follow synthetic undo/redo links from OPERATION-RECORD to a real operation."
  (let ((current operation-record)
        (seen nil)
        restored-id)
    (while (setq restored-id (majjic--synthetic-operation-restored-id current))
      (when (member restored-id seen)
        (user-error "Operation restore chain has a cycle"))
      (push restored-id seen)
      (setq current (majjic--read-operation-record restored-id)))
    current))

(defun majjic--effective-undo-operation-preview ()
  "Return preview text describing what `jj undo' will effectively do."
  (let* ((subject (majjic--read-operation-record))
         (subject (if (majjic--synthetic-operation-restored-id subject)
                      (majjic--terminal-non-synthetic-operation-record subject)
                    subject))
         (parent-ids (majjic-operation-record-parent-ids subject))
         (_destination (pcase (length parent-ids)
                         (0 (user-error "Cannot undo root operation"))
                         (1 (majjic--read-operation-record (car parent-ids)))
                         (_ (user-error "Cannot undo a merge operation")))))
    (string-join
     (list "Will undo operation:"
           (majjic--operation-log-entry (majjic-operation-record-id subject) t))
     "\n")))

(defun majjic--effective-redo-operation-preview ()
  "Return preview text describing what `jj redo' will effectively do."
  (let* ((cursor (majjic--read-operation-record))
         (cursor (if-let* ((restored-id (majjic--operation-restored-id
                                         cursor majjic--redo-op-desc-prefix)))
                     (majjic--read-operation-record restored-id)
                   cursor)))
    (unless (string-prefix-p majjic--undo-op-desc-prefix
                             (majjic-operation-record-description cursor))
      (user-error "Nothing to redo"))
    (let* ((parent-ids (majjic-operation-record-parent-ids cursor))
           (destination (if (= (length parent-ids) 1)
                            (majjic--read-operation-record (car parent-ids))
                          (user-error "Undo operation should have a single parent")))
           (subject (if (majjic--synthetic-operation-restored-id destination)
                        (majjic--terminal-non-synthetic-operation-record destination)
                      destination)))
      (string-join
       (list "Will redo operation:"
             (majjic--operation-log-entry (majjic-operation-record-id subject) t))
       "\n"))))

(defun majjic--rebase-moved-change-ids (source-change-id source-mode)
  "Return change ids moved by rebasing SOURCE-CHANGE-ID in SOURCE-MODE."
  (let ((sources (cond
                  ((null source-change-id) nil)
                  ((listp source-change-id) source-change-id)
                  (t (list source-change-id)))))
    (if (eq source-mode 'descendants)
        (cl-remove-duplicates
         (apply #'append
                (mapcar (lambda (source)
                          (seq-remove #'string-empty-p
                                      (split-string
                                       (string-trim
                                        (majjic--call-jj majjic--repo-root "log"
                                                         "-r" (format "(%s)::" source)
                                                         "--no-graph" "--color" "never"
                                                         "--template" "change_id ++ \"\\n\""
                                                         "--ignore-working-copy" "--no-pager"))
                                       "\n" t)))
                        sources))
         :test #'equal)
      sources)))

(defun majjic--rebase-args (source target target-mode &optional source-mode)
  "Return `jj rebase' arguments for SOURCE onto/after/before TARGET.
TARGET-MODE is one of `onto', `after', or `before'.
SOURCE-MODE is one of `revision' or `descendants', defaulting to `revision'."
  (append (list "rebase")
          (apply #'append
                 (mapcar (lambda (source-id)
                           (list (if (eq source-mode 'descendants) "--source" "--revisions")
                                 source-id))
                         (cond
                          ((null source) nil)
                          ((listp source) source)
                          (t (list source)))))
          (pcase target-mode
            ('after (list "--insert-after" target))
            ('before (list "--insert-before" target))
            (_ (list "--onto" target)))))

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

(provide 'majjic-jj)

;;; majjic-jj.el ends here
