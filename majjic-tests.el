;;; majjic-tests.el --- Lightweight tests for Majjic -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Small seam-focused tests for the Majjic refactor.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'majjic)

(defun majjic-test--kill-demo-buffer ()
  "Remove any existing demo log buffer so tests start with clean UI state."
  (when-let* ((buffer (get-buffer "majjic: majjic-demo")))
    (kill-buffer buffer)))

(ert-deftest majjic-test-parse-root-and-trailing-elided ()
  "Root revisions should not synthesize a summary, and trailing `~' survives."
  (let* ((sep majjic--record-separator)
         (output (concat sep "zzzzzzzz" sep "00000000" sep "◆ zzzzzzzz root() 00000000\n"
                         "│\n"
                         "~\n"))
         (records (majjic--parse-log-output output)))
    (should (= (length records) 2))
    (should (eq (majjic-revision-kind (nth 0 records)) 'revision))
    (should-not (majjic-revision-summary (nth 0 records)))
    (should (eq (majjic-revision-kind (nth 1 records)) 'graph))
    (should (equal (majjic-revision-heading (nth 1 records)) "~"))))

(ert-deftest majjic-test-parse-summary-and-multi-hunk-diff ()
  "Summary parsing should preserve rename rows and parse multiple hunks."
  (let* ((changes (majjic--parse-file-summary-output
                   "M app.py\nR {old.txt => new.txt}"))
         (hunks (majjic--parse-file-diff-output
                 (string-join
                  '("diff --git a/app.py b/app.py"
                    "index 1111111..2222222 100644"
                    "--- a/app.py"
                    "+++ b/app.py"
                    "@@ -1,2 +1,2 @@"
                    "-old"
                    "+new"
                    "@@ -10 +10 @@"
                    "-x"
                    "+y")
                  "\n"))))
    (should (= (length changes) 2))
    (should (equal (majjic-file-change-path (nth 0 changes)) "app.py"))
    (should (majjic-file-change-rename (nth 1 changes)))
    (should (= (length hunks) 2))
    (should (= (majjic-hunk-index (nth 0 hunks)) 0))
    (should (= (majjic-hunk-index (nth 1 hunks)) 1))
    (should (= (majjic-hunk-old-start (nth 1 hunks)) 10))
    (should (= (majjic-hunk-new-start (nth 1 hunks)) 10))))

(ert-deftest majjic-test-refresh-preserves-selection-and-expansion ()
  "A rerender should preserve the selected revision and expanded revision state."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (let ((top-prefix (majjic--rebase-target-summary-prefix
                         (majjic--current-revision-section) 'after)))
        (should-not (string-match-p "│" top-prefix)))
      (goto-char (point-min))
      (should (search-forward "Edit notes in two distant hunks" nil t))
      (beginning-of-line)
      (let ((selected (majjic--current-change-id)))
        (majjic-toggle-at-point)
        (should (= (length (majjic--expanded-change-ids)) 1))
        (majjic--log-refresh-sync (majjic--capture-refresh-state))
        (should (equal (majjic--current-change-id) selected))
        (should (= (length (majjic--expanded-change-ids)) 1))))))

(ert-deftest majjic-test-rebase-command-args ()
  "Rebase command args should map target modes to the expected jj flags."
  (should (equal (majjic--rebase-args "src" "dst" 'onto)
                 '("rebase" "-r" "src" "--onto" "dst")))
  (should (equal (majjic--rebase-args "src" "dst" 'after)
                 '("rebase" "-r" "src" "--insert-after" "dst")))
  (should (equal (majjic--rebase-args "src" "dst" 'before)
                 '("rebase" "-r" "src" "--insert-before" "dst"))))

(ert-deftest majjic-test-rebase-mode-preview-and-cancel ()
  "Entering rebase mode should show transient overlays and cancel should clear them."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (should (search-forward "Edit notes in two distant hunks" nil t))
      (beginning-of-line)
      (let ((source (majjic--current-change-id)))
        (majjic-rebase-start)
        (should majjic-rebase-mode)
        (should (equal (majjic-rebase-state-source-change-id majjic--rebase-state)
                       (majjic--change-id-for-commit-id source)))
        (let* ((revision (majjic--current-revision-section))
               (marker-overlay (seq-find (lambda (overlay)
                                           (let ((text (overlay-get overlay 'before-string)))
                                             (and (overlay-start overlay)
                                                  (stringp text)
                                                  (string-match-p "<< move >>" text))))
                                         majjic--rebase-overlays)))
          (should marker-overlay)
          (should (= (overlay-start marker-overlay)
                     (majjic--revision-marker-position revision))))
        (let* ((marker-overlay (seq-find (lambda (overlay)
                                           (let ((text (overlay-get overlay 'before-string)))
                                             (and (stringp text)
                                                  (string-match-p "<< move >>" text))))
                                         majjic--rebase-overlays))
               (marker-text (overlay-get marker-overlay 'before-string)))
          (should (equal (get-text-property 0 'face marker-text)
                         '(:inherit (shadow magit-section-highlight) :weight bold))))
        (majjic-section-forward)
        (should (seq-some (lambda (overlay)
                            (let ((text (or (overlay-get overlay 'before-string)
                                            (overlay-get overlay 'after-string))))
                              (and (overlay-start overlay)
                                   (stringp text)
                                   (string-match-p "<< onto >>" text))))
                          majjic--rebase-overlays))
        (let* ((onto-overlay (seq-find (lambda (overlay)
                                         (let ((text (overlay-get overlay 'before-string)))
                                           (and (stringp text)
                                                (string-match-p "<< onto >>" text))))
                                       majjic--rebase-overlays))
               (onto-text (overlay-get onto-overlay 'before-string)))
          (should (equal (aref onto-text 0) ?│))
          (should (equal (get-text-property 0 'face onto-text) 'default))
          (should (equal (get-text-property (string-match "<<" onto-text) 'face onto-text)
                         '(:inherit shadow :weight bold))))
        (majjic-abandon-start)
        (should (eq (majjic-rebase-state-target-mode majjic--rebase-state) 'after))
        (when-let* ((target (majjic--current-revision-section))
                    (next (majjic--next-top-level-section target)))
          (should (majjic--previous-top-level-section next))
          (should (equal (majjic--rebase-target-summary-prefix target 'before)
                         (majjic--rebase-target-summary-prefix next 'after))))
        (majjic-rebase-set-before)
        (majjic-toggle-at-point)
        (let* ((target (majjic--current-revision-section))
               (before-overlay (seq-find (lambda (overlay)
                                           (let ((text (overlay-get overlay 'after-string)))
                                             (and (overlay-start overlay)
                                                  (stringp text)
                                                  (string-match-p "<< before >>" text))))
                                         majjic--rebase-overlays)))
          (should before-overlay)
          (should (> (overlay-start before-overlay)
                     (oref target content))))
        (let ((called nil))
          (cl-letf (((symbol-function 'majjic--run-mutation)
                     (lambda (&rest _args) (setq called t))))
            (majjic--goto-change source)
            (should-error (majjic-rebase-apply) :type 'user-error)
            (should-not called)))
        (majjic-rebase-cancel)
        (should-not majjic-rebase-mode)
        (should-not majjic--rebase-state)
        (should-not majjic--rebase-overlays)))))

(ert-deftest majjic-test-rebase-apply-moves-source-and-keeps-it-selected ()
  "Applying rebase in the UI should move the source and keep it selected."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-rebase-" t))
         (default-directory repo))
    (cl-labels ((jj (&rest args)
                    (let ((default-directory repo))
                      (with-temp-buffer
                        (let ((exit (apply #'process-file "jj" nil t nil args)))
                          (unless (zerop exit)
                            (error "jj %S failed: %s" args (buffer-string)))
                          (string-trim (buffer-string)))))))
      (jj "git" "init" ".")
      (jj "config" "set" "--repo" "user.name" "Test User")
      (jj "config" "set" "--repo" "user.email" "test@example.com")
      (with-temp-file (expand-file-name "f.txt" repo)
        (insert "root\n"))
      (jj "describe" "-m" "root")
      (let ((root (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id")))
        (jj "new")
        (with-temp-file (expand-file-name "f.txt" repo)
          (insert "child\n"))
        (jj "describe" "-m" "child")
        (let* ((child (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id"))
               (child-change (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "change_id")))
          (jj "new" root)
          (with-temp-file (expand-file-name "g.txt" repo)
            (insert "side\n"))
          (jj "describe" "-m" "side")
          (let ((side (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id")))
            (majjic)
            (with-current-buffer (majjic--log-buffer-name repo)
              (goto-char (point-min))
              (should (search-forward "child" nil t))
              (beginning-of-line)
              (majjic-rebase-start)
              (goto-char (point-min))
              (should (search-forward "side" nil t))
              (beginning-of-line)
              (majjic-rebase-apply)
              (should (equal (majjic--change-id-for-commit-id (majjic--current-change-id))
                             child-change))
              (should-not majjic-rebase-mode)
              (should (equal (jj "log" "-r" (format "parents(%s)" (majjic--current-change-id))
                                 "--no-graph" "--color" "never" "--template" "commit_id")
                             side)))))))))

(provide 'majjic-tests)

;;; majjic-tests.el ends here
