;;; majjic-render.el --- Rendering helpers for Majjic -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andy Phan
;; Package-Requires: ((emacs "28.1") (magit-section "4.5.0"))
;; Keywords: tools, vc

;;; Commentary:

;; Text and face helpers used when rendering Majjic sections.

;;; Code:

(require 'ansi-color)
(require 'color)
(require 'magit-section)
(require 'majjic-model)
(require 'subr-x)

(declare-function majjic--current-revision-section "majjic-actions")
(declare-function majjic--commit-id-for-change-id "majjic-jj")
(defvar majjic--rebase-overlays)
(defvar majjic--rebase-state)
(defvar majjic-rebase-mode)

(defun majjic--revision-heading-text (record)
  "Return propertized heading text for revision RECORD."
  (majjic--ansi-colorize (majjic-revision-heading record)))

(defun majjic--body-text (string)
  "Return propertized body STRING, preserving color."
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
  "Insert file heading STRING with a colorized status marker."
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

(defun majjic--summary-status-width ()
  "Return the fixed width for human-readable summary status labels."
  (apply #'max (mapcar (lambda (pair) (length (cdr pair)))
                       majjic--summary-status-labels)))

(defun majjic--format-summary-line (line width)
  "Return summary LINE with a padded human-readable status column of WIDTH."
  (let ((plain (majjic--strip-ansi line)))
    (if (string-match "\\`\\([[:space:]│├└┼┤┬╯╮╭╰─]*\\)\\(\\S-+\\)\\([[:space:]]+\\)\\(.*\\)\\'" plain)
        (let* ((prefix (match-string 1 plain))
               (status (match-string 2 plain))
               (label (majjic--summary-status-label status))
               (path (match-string 4 plain)))
          (format "%s%s %s" prefix (string-pad label width) path))
      line)))

(defun majjic--apply-hunk-heading-background (start end)
  "Overlay a darker background on the hunk heading from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face (majjic--hunk-heading-face))
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun majjic--hunk-heading-face (&optional selected)
  "Return a theme-relative face for hunk headings."
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
    (unless has-face
      (put-text-property 0 (length colored) 'face 'default colored))
    colored))

(defun majjic--update-selected-hunk-heading ()
  "Bold hunk headers according to the currently selected section."
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

(defun majjic--insert-records (records expanded-change-ids expanded-file-keys)
  "Insert RECORDS, restoring expanded revisions and files from saved state."
  (magit-insert-section (majjic-root-section)
    (dolist (record records)
      (pcase (majjic-revision-kind record)
        ('elided
         (magit-insert-section (majjic-elided-section)
           (magit-insert-heading (majjic--ansi-colorize (majjic-revision-heading record)))))
        ('graph
         (magit-insert-section (majjic-graph-line-section)
           (magit-insert-heading (majjic--ansi-colorize (majjic-revision-heading record)))))
        (_
         (let* ((commit-id (majjic-revision-commit-id record))
                (expanded (member commit-id expanded-change-ids)))
           (magit-insert-section (majjic-revision-section commit-id)
             (magit-insert-heading (majjic--revision-heading-text record))
             (when-let* ((summary (majjic-revision-summary record)))
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

(defun majjic--insert-file-summary (commit-id expanded-file-keys)
  "Insert a lazily loaded file summary for COMMIT-ID.
Restore expanded file diffs listed in EXPANDED-FILE-KEYS."
  (let* ((prefix (majjic--file-summary-prefix))
         (output (string-trim-right
                  (majjic--call-jj majjic--repo-root "diff" "--summary"
                                   "--color" "always" "-r" commit-id)))
         (file-changes (majjic--parse-file-summary-output output))
         (status-width (majjic--summary-status-width)))
    (if (string-empty-p output)
        (insert (majjic--body-text (concat prefix "(no files changed)")) "\n")
      (dolist (file-change file-changes)
        (let* ((raw-line (majjic-file-change-raw-line file-change))
               (heading-line (concat prefix (majjic--format-summary-line raw-line status-width)))
               (color-source (concat prefix raw-line)))
          (if (majjic-file-change-rename file-change)
              (magit-insert-section (majjic-rename-section nil nil :commit-id commit-id)
                (majjic--insert-file-heading heading-line color-source))
            (let* ((path (majjic-file-change-path file-change))
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
         (hunks (majjic--parse-file-diff-output output)))
    (if hunks
        (dolist (hunk hunks)
          (magit-insert-section (majjic-hunk-section (majjic-hunk-index hunk) nil
                                                     :old-start (majjic-hunk-old-start hunk)
                                                     :old-count (majjic-hunk-old-count hunk)
                                                     :new-start (majjic-hunk-new-start hunk)
                                                     :new-count (majjic-hunk-new-count hunk)
                                                     :body-prefix prefix
                                                     :body-lines (majjic-hunk-body-lines hunk))
            (let ((heading-start (point)))
              (magit-insert-heading
               (majjic--body-text (concat prefix (majjic-hunk-header hunk))))
              (majjic--apply-hunk-heading-background heading-start (point)))
            (magit-insert-section-body
              (majjic--insert-hunk-body-lines prefix (majjic-hunk-body-lines hunk)))))
      (insert (majjic--body-text (concat prefix "(no diff)")) "\n"))))

(defun majjic--insert-hunk-body-lines (prefix body-lines)
  "Insert BODY-LINES for a hunk using PREFIX and preserve color."
  (dolist (body-line body-lines)
    (insert (majjic--body-text (concat prefix body-line)) "\n")))

(defun majjic--prepare-missing-hunk-body (hunk)
  "If HUNK lost its body text while hidden, regenerate it on the next show."
  (when (and (= (oref hunk content) (oref hunk end))
             (null (oref hunk washer))
             (oref hunk body-lines))
    (let ((prefix (or (oref hunk body-prefix) ""))
          (body-lines (oref hunk body-lines)))
      (oset hunk washer
            (lambda ()
              (majjic--insert-hunk-body-lines prefix body-lines))))))

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
  "Turn graph heading PREFIX into a continuation prefix for child rows."
  (apply #'string
         (mapcar (lambda (char)
                   (cond
                    ((eq char ?│) ?│)
                    ((memq char '(?├ ?┼ ?┤ ?┬ ?┌ ?┐ ?╭ ?╮)) ?│)
                    ((eq char ?\s) ?\s)
                    ((eq char ?\t) ?\t)
                    (t ?\s)))
                 (string-to-list prefix))))

(defun majjic--clear-rebase-overlays ()
  "Remove all transient rebase preview overlays from the current buffer."
  (mapc #'delete-overlay majjic--rebase-overlays)
  (setq majjic--rebase-overlays nil))

(defun majjic--sync-rebase-overlays ()
  "Rebuild rebase preview overlays for the current point and mode state."
  (majjic--clear-rebase-overlays)
  (when (and majjic-rebase-mode
             majjic--rebase-state
             (bound-and-true-p magit-root-section))
    (let ((moved-change-ids (or (majjic-rebase-state-moved-change-ids majjic--rebase-state)
                                (list (majjic-rebase-state-source-change-id majjic--rebase-state)))))
      (dolist (revision (oref magit-root-section children))
        (when (and (object-of-class-p revision 'majjic-revision-section)
                   (majjic--revision-section-moved-p revision moved-change-ids))
          (majjic--add-rebase-source-overlay revision))))
    (when-let* ((target-section (majjic--current-revision-section)))
      (majjic--add-rebase-target-overlay
       target-section
       (majjic-rebase-state-source-change-id majjic--rebase-state)
       (oref target-section value)
       (majjic-rebase-state-source-mode majjic--rebase-state)
       (majjic-rebase-state-target-mode majjic--rebase-state)))))

(defun majjic--revision-section-by-id (commit-id)
  "Return the visible revision section for COMMIT-ID, or nil."
  (when (and commit-id (bound-and-true-p magit-root-section))
    (seq-find (lambda (child)
                (and (object-of-class-p child 'majjic-revision-section)
                     (equal (oref child value) commit-id)))
              (oref magit-root-section children))))

(defun majjic--add-rebase-source-overlay (section)
  "Add the source marker overlay to SECTION."
  (when (markerp (oref section start))
    (let ((overlay (majjic--make-rebase-anchor-overlay
                    (majjic--revision-marker-position section)))
          (selected (eq section (majjic--current-revision-section))))
      (overlay-put overlay 'before-string
                   (propertize "<< move >> "
                               'face (if selected
                                         '(:inherit (shadow magit-section-highlight)
                                           :weight bold)
                                       '(:inherit shadow :weight bold))))
      (overlay-put overlay 'priority 1003)
      (overlay-put overlay 'evaporate t)
      (push overlay majjic--rebase-overlays))))

(defun majjic--add-rebase-target-overlay (section source target source-mode target-mode)
  "Add the target summary overlay to SECTION for SOURCE, TARGET, and TARGET-MODE.
SOURCE-MODE controls whether the summary says revision or descendants."
  (when (and source target (markerp (oref section start)) (markerp (oref section content)))
    (let* ((label (majjic--rebase-target-mode-label target-mode))
           (prefix (majjic--rebase-target-summary-prefix section target-mode))
           (source-text (if (eq source-mode 'descendants)
                            "rebase itself and descendants of "
                          "rebase revision "))
           (summary (concat (propertize (format "<< %s >>" label)
                                        'face '(:inherit shadow :weight bold))
                            " "
                            (propertize source-text 'face 'shadow)
                            (propertize (majjic--short-change-id source) 'face 'font-lock-keyword-face)
                            " "
                            (propertize label 'face 'shadow)
                            " "
                            (propertize (majjic--short-change-id target) 'face 'font-lock-keyword-face)
                            "\n"))
           (summary-text (concat (propertize prefix 'face 'default) summary))
           (pos (if (eq target-mode 'before)
                    (max (oref section start) (1- (oref section end)))
                  (oref section start)))
           (overlay (majjic--make-rebase-anchor-overlay pos)))
      (overlay-put overlay (if (eq target-mode 'before) 'after-string 'before-string)
                   summary-text)
      (overlay-put overlay 'priority 1003)
      (overlay-put overlay 'evaporate t)
      (push overlay majjic--rebase-overlays))))

(defun majjic--make-rebase-anchor-overlay (pos)
  "Return a live non-empty overlay anchored at POS for transient rebase text."
  (make-overlay pos (min (point-max) (1+ pos)) nil t t))

(defun majjic--rebase-target-mode-label (target-mode)
  "Return display label for TARGET-MODE."
  (pcase target-mode
    ('after "after")
    ('before "before")
    (_ "onto")))

(defun majjic--short-change-id (commit-id)
  "Return a compact display form of COMMIT-ID."
  (if (stringp commit-id)
      (substring commit-id 0 (min 8 (length commit-id)))
    ""))

(defun majjic--revision-marker-position (section)
  "Return the position just after SECTION's graph gutter."
  (save-excursion
    (goto-char (oref section start))
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (+ (line-beginning-position)
         (if (string-match "\\`[^[:alnum:]]*" line)
             (match-end 0)
           0)))))

(defun majjic--revision-section-change-id (section)
  "Return the visible change id text from revision SECTION's heading."
  (save-excursion
    (goto-char (oref section start))
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (when (string-match "\\`[^[:alnum:]]*\\([[:alnum:]]+\\)" line)
        (match-string 1 line)))))

(defun majjic--revision-section-moved-p (section moved-change-ids)
  "Return non-nil if SECTION's visible change id matches MOVED-CHANGE-IDS."
  (when-let* ((visible-change-id (majjic--revision-section-change-id section)))
    (seq-some (lambda (change-id)
                (or (equal visible-change-id change-id)
                    (string-prefix-p visible-change-id change-id)
                    (string-prefix-p change-id visible-change-id)))
              moved-change-ids)))

(defun majjic--rebase-target-summary-prefix (section target-mode)
  "Return a graph prefix for a transient target summary near SECTION.
Mirror upstream jjui's gap model: for `before', extend the selected row across
the gap below it; for `after' and `onto', extend the previous row across the
gap above the selected row."
  (let* ((width (length (majjic--heading-gutter-text section)))
         (source (if (eq target-mode 'before)
                     section
                   (majjic--previous-top-level-section section))))
    (if source
        (majjic--section-extended-gutter-prefix source width)
      (make-string width ?\s))))

(defun majjic--section-extended-gutter-prefix (section width)
  "Return SECTION's branch continuation gutter extended to WIDTH columns."
  (let ((mask (make-vector width nil)))
    (save-excursion
      (goto-char (oref section start))
      (while (< (point) (oref section end))
        (let ((gutter (majjic--line-gutter-text width)))
          (dotimes (i width)
            (pcase (majjic--graph-extend-state (aref gutter i))
              ('yes (aset mask i t))
              ('no (aset mask i nil))
              ('carry nil))))
        (forward-line 1)))
    (apply #'string
           (cl-loop for alive across mask
                    collect (if alive ?│ ?\s)))))

(defun majjic--line-gutter-text (width)
  "Return WIDTH chars of current-line gutter text, padded with spaces."
  (let* ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (gutter (substring text 0 (min width (length text)))))
    (concat gutter (make-string (max 0 (- width (length gutter))) ?\s))))

(defun majjic--graph-extend-state (char)
  "Return how graph CHAR affects branch continuation across a gap.
The return value is one of `yes', `no', or `carry', following upstream jjui's
row extension semantics."
  (cond
   ((memq char '(?│ ?| ?╭ ?├ ?┐ ?┤ ?┌ ?╮ ?┬ ?┼ ?+ ?\\ ?.)) 'yes)
   ((memq char '(?╯ ?╰ ?└ ?┴ ?┘ ?\s ?/)) 'no)
   ((memq char '(?─ ?-)) 'carry)
   (t 'no)))

(defun majjic--heading-gutter-text (section)
  "Return the graph gutter text from SECTION's heading line."
  (or (when section
        (save-excursion
          (goto-char (oref section start))
          (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
            (when (string-match "\\`\\([^[:alnum:]]*\\)" line)
              (match-string 1 line)))))
      ""))

(defun majjic--next-top-level-section (section)
  "Return the next top-level sibling section after SECTION, or nil."
  (when-let* ((parent (oref section parent))
              (siblings (oref parent children)))
    (cadr (memq section siblings))))

(defun majjic--previous-top-level-section (section)
  "Return the previous top-level sibling section before SECTION, or nil."
  (when-let* ((parent (oref section parent))
              (siblings (oref parent children)))
    (let ((previous nil))
      (catch 'found
        (dolist (sibling siblings)
          (when (eq sibling section)
            (throw 'found previous))
          (setq previous sibling))
        nil))))

(provide 'majjic-render)

;;; majjic-render.el ends here
