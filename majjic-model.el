;;; majjic-model.el --- Majjic model and UI state -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andy Phan
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc

;;; Commentary:

;; Plain data types used by `majjic' to separate parsed repository data and
;; transient UI state from buffer rendering.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'subr-x)

(cl-defstruct majjic-state
  "Captured UI state for a `majjic' refresh."
  current-commit-id
  current-change-id
  commit-change-ids
  expanded-commit-ids
  expanded-file-keys
  marked-change-ids
  rebase-state)

(cl-defstruct majjic-rebase-state
  "Transient state for `majjic-rebase-mode'."
  source-change-id
  source-mode
  moved-change-ids
  target-mode)

(cl-defstruct majjic-revision
  "Parsed revision row from `jj log'."
  kind
  change-id
  commit-id
  heading
  summary)

(cl-defstruct majjic-file-change
  "Parsed file summary row from `jj diff --summary'."
  status
  path
  raw-line
  rename)

(cl-defstruct majjic-hunk
  "Parsed unified diff hunk metadata and body."
  index
  header
  body-lines
  old-start
  old-count
  new-start
  new-count)

(defun majjic--parse-file-summary-line (line)
  "Parse a `jj diff --summary' LINE into a `majjic-file-change'."
  (make-majjic-file-change
   :status (majjic--summary-status-token line)
   :path (majjic--summary-line-path line)
   :raw-line line
   :rename (majjic--rename-summary-line-p line)))

(defun majjic--parse-file-summary-output (output)
  "Parse `jj diff --summary' OUTPUT into `majjic-file-change' structs."
  (mapcar #'majjic--parse-file-summary-line
          (unless (string-empty-p output)
            (split-string output "\n"))))

(defun majjic--summary-status-token (line)
  "Return the summary status token from LINE, or nil if none."
  (let ((plain (string-trim-left (majjic--strip-ansi line))))
    (car (split-string plain "[[:space:]]+" t))))

(defun majjic--parse-file-diff-output (output)
  "Parse unified diff OUTPUT into `majjic-hunk' structs.
Skip the git preamble and keep only hunk headers plus bodies."
  (let ((lines (if (string-empty-p output) nil (split-string output "\n")))
        hunks
        hunk-header
        hunk-body
        (hunk-index 0))
    (cl-labels ((flush-hunk ()
                  (when hunk-header
                    (pcase-let* ((body-lines (nreverse hunk-body))
                                 (`(,old-start ,old-count ,new-start ,new-count)
                                  (or (majjic--parse-hunk-header hunk-header)
                                      (list nil nil nil nil))))
                      (push (make-majjic-hunk
                             :index hunk-index
                             :header hunk-header
                             :body-lines body-lines
                             :old-start old-start
                             :old-count old-count
                             :new-start new-start
                             :new-count new-count)
                            hunks)
                      (setq hunk-index (1+ hunk-index))
                      (setq hunk-header nil)
                      (setq hunk-body nil)))))
      (dolist (line lines)
        (if (majjic--hunk-header-line-p line)
            (progn
              (flush-hunk)
              (setq hunk-header line))
          (when hunk-header
            (push line hunk-body))))
      (flush-hunk))
    (nreverse hunks)))

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

(defun majjic--hunk-header-line-p (line)
  "Return non-nil if LINE is a unified diff hunk header."
  (string-prefix-p "@@ " (majjic--strip-ansi line)))

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

(defun majjic--parse-log-output (output)
  "Parse `jj log --summary' OUTPUT into `majjic-revision' structs."
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
        nil)))
    (when current
      (push (nreverse current) records))
    (nreverse (mapcar #'majjic--record-alist-to-revision records))))

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

(defun majjic--record-alist-to-revision (record)
  "Convert parsed RECORD alist into a `majjic-revision' struct."
  (if-let* ((elided (alist-get 'elided record)))
      (make-majjic-revision :kind 'elided :heading elided)
    (if-let* ((graph (alist-get 'graph record)))
        (make-majjic-revision :kind 'graph :heading graph)
      (let* ((heading-entry (alist-get 'heading record))
             (commit-id (plist-get heading-entry :commit-id))
             (summary (or (alist-get 'summary record) "")))
        (when (and (majjic--root-commit-id-p commit-id)
                   (string-match-p "(no description set)" summary))
          (setq summary nil))
        (make-majjic-revision
         :kind 'revision
         :change-id (plist-get heading-entry :change-id)
         :commit-id commit-id
         :heading (plist-get heading-entry :text)
         :summary summary)))))

(defun majjic--normalize-summary-line (line)
  "Ensure blank summary LINE still shows a placeholder description."
  (if (string-match-p "[[:alnum:]]" line)
      line
    (concat line "(no description set)")))

(defun majjic--root-commit-id-p (commit-id)
  "Return non-nil if COMMIT-ID is Jujutsu's synthetic root commit."
  (and (stringp commit-id)
       (string-match-p "\\`0+\\'" commit-id)))

(defun majjic--elided-line-p (line)
  "Return non-nil if LINE is Jujutsu's synthetic elided-revisions row."
  (string-match-p "(elided revisions)" line))

(defun majjic--graph-continuation-line-p (line)
  "Return non-nil if LINE is a graph-only junction row from `jj log'."
  (let ((plain (string-trim (majjic--strip-ansi line))))
    (and (not (string-empty-p plain))
         (not (string-match-p "[[:alnum:]]" plain))
         (or (string= plain "~")
             (string-match-p "[├└┼┤┬╯╮╭╰─]" plain)))))

(defun majjic--strip-ansi (string)
  "Return STRING with ANSI escape sequences removed."
  (with-temp-buffer
    (insert string)
    (ansi-color-filter-region (point-min) (point-max))
    (buffer-string)))

(provide 'majjic-model)

;;; majjic-model.el ends here
