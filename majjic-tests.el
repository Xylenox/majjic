;;; majjic-tests.el --- Lightweight tests for Majjic -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Small seam-focused tests for the Majjic refactor.

;;; Code:

(require 'ert)
(require 'majjic)

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
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (should (search-forward "Edit notes in two distant hunks" nil t))
      (beginning-of-line)
      (let ((selected (majjic--current-change-id)))
        (majjic-toggle-at-point)
        (should (= (length (majjic--expanded-change-ids)) 1))
        (majjic--log-refresh-sync (majjic--capture-refresh-state))
        (should (equal (majjic--current-change-id) selected))
        (should (= (length (majjic--expanded-change-ids)) 1))))))

(provide 'majjic-tests)

;;; majjic-tests.el ends here
