;;; majjic-actions.el --- Navigation helpers for Majjic -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andy Phan
;; Package-Requires: ((emacs "28.1") (magit-section "4.5.0"))
;; Keywords: tools, vc

;;; Commentary:

;; Section lookup and movement helpers for `majjic'.  Command entrypoints still
;; live in `majjic.el' with the mode/keymap definitions.

;;; Code:

(require 'magit-section)

(defun majjic--current-revision-section ()
  "Return the revision section at point, or its revision ancestor."
  (majjic--current-section-of-class 'majjic-revision-section))

(defun majjic--current-file-section ()
  "Return the file section at point, or its file ancestor."
  (majjic--current-section-of-class 'majjic-file-section))

(defun majjic--current-hunk-section ()
  "Return the hunk section at point, or nil."
  (majjic--current-section-of-class 'majjic-hunk-section))

(defun majjic--current-section-of-class (class)
  "Return the section at point, or its nearest ancestor of CLASS."
  (let ((section (magit-section-at)))
    (while (and section (not (object-of-class-p section class)))
      (setq section (oref section parent)))
    section))

(defun majjic--current-commit-id ()
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

(provide 'majjic-actions)

;;; majjic-actions.el ends here
