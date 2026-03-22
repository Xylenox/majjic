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

(declare-function majjic--current-change-id "majjic-actions")
(declare-function majjic--expanded-change-ids "majjic")
(declare-function majjic--expanded-file-keys "majjic")
(declare-function majjic--log-refresh-sync "majjic")
(declare-function majjic--parse-log-output "majjic-model")
(declare-function majjic--revision-commit-ids "majjic")

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
         (current-state (majjic--capture-refresh-state))
         (target (majjic-state-current-change-id current-state)))
    (unwind-protect
        (condition-case err
            (progn
              (funcall thunk)
              (when after-success
                (funcall after-success))
              (setq target (cond
                            ((functionp target-spec) (funcall target-spec))
                            (target-spec target-spec)
                            (t (majjic-state-current-change-id current-state))))
              (setf (majjic-state-current-change-id current-state) target)
              (majjic--log-refresh-sync current-state))
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

(defun majjic--rebase-args (source target target-mode)
  "Return `jj rebase' arguments for SOURCE onto/after/before TARGET.
TARGET-MODE is one of `onto', `after', or `before'."
  (append (list "rebase" "-r" source)
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
