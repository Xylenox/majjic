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

(defun majjic-test--process-live-p (process)
  "Return non-nil if PROCESS is live."
  (and (processp process)
       (process-live-p process)))

(defun majjic-test--section-load-live-p (&optional section)
  "Return non-nil if SECTION or any descendant has a live async load."
  (when-let* ((section (or section (and (bound-and-true-p magit-root-section)
                                        magit-root-section))))
    (or (and (ignore-errors (slot-exists-p section 'load-process))
             (majjic-test--process-live-p (slot-value section 'load-process)))
        (seq-some #'majjic-test--section-load-live-p
                  (oref section children)))))

(defun majjic-test--wait-for (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, up to TIMEOUT seconds."
  (let* ((timeout (or timeout 5))
         (deadline (+ (float-time) timeout)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun majjic-test--wait-for-idle ()
  "Wait for the current Majjic buffer to finish async work."
  (majjic-test--wait-for
   (lambda ()
     (and (not majjic--mutation-in-progress)
          (not majjic--preview-in-progress)
          (not (majjic-test--process-live-p majjic--mutation-process))
          (not (majjic-test--process-live-p majjic--preview-process))
          (not (majjic-test--process-live-p majjic--refresh-process))
          (not (majjic-test--section-load-live-p))))))

(defun majjic-test--section-body-string (section)
  "Return SECTION's body text without properties."
  (buffer-substring-no-properties (oref section content) (oref section end)))

(defun majjic-test--jj (repo &rest args)
  "Run jj with ARGS in REPO and return stdout."
  (with-temp-buffer
    (let ((default-directory repo))
      (let ((exit (apply #'process-file "jj" nil t nil args)))
        (unless (zerop exit)
          (error "jj %S failed: %s" args (buffer-string)))))
    (string-trim-right (buffer-string))))

(defun majjic-test--configure-jj-repo (repo)
  "Configure user identity in the temp jj REPO."
  (majjic-test--jj repo "config" "set" "--repo" "user.name" "Test User")
  (majjic-test--jj repo "config" "set" "--repo" "user.email" "test@example.com"))

(defun majjic-test--write-repo-file (repo path contents)
  "Write CONTENTS to PATH under REPO."
  (let ((file (expand-file-name path repo)))
    (make-directory (file-name-directory file) t)
    (write-region contents nil file nil 'silent)))

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
      (let ((selected (majjic--current-commit-id)))
        (majjic-toggle-at-point)
        (should (= (length (majjic--expanded-commit-ids)) 1))
        (majjic--log-refresh-sync (majjic--capture-refresh-state))
        (should (equal (majjic--current-commit-id) selected))
        (should (= (length (majjic--expanded-commit-ids)) 1))))))

(ert-deftest majjic-test-render-refreshes-selection-highlight ()
  "A rerender should recreate the Magit highlight for the selected row."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((inhibit-read-only t))
      (majjic--render-records
       (list (make-majjic-revision
              :kind 'revision
              :change-id "change-1"
              :commit-id "commit-1"
              :heading "◆ commit-1 test"
              :summary "test revision"))
       (make-majjic-state)))
    (should (majjic--current-revision-section))
    (should (seq-some
             (lambda (overlay)
               (eq (overlay-get overlay 'font-lock-face)
                   'magit-section-highlight))
             magit-section-highlight-overlays))))

(ert-deftest majjic-test-refresh-state-remaps-rewritten-current-and-expanded-revisions ()
  "Refresh state should follow a selected expanded revision after its commit id changes."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((old-record (make-majjic-revision
                       :kind 'revision
                       :change-id "change-1"
                       :commit-id "old-commit"
                       :heading "◆ old-commit test"
                       :summary "test revision"))
          (new-record (make-majjic-revision
                       :kind 'revision
                       :change-id "change-1"
                       :commit-id "new-commit"
                       :heading "◆ new-commit test"
                       :summary "test revision"))
          state)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir _callback &rest _args)
                   'summary-process)))
        (let ((inhibit-read-only t))
          (majjic--render-records (list old-record) (make-majjic-state)))
        (majjic-toggle-at-point)
        (setq state (majjic--capture-refresh-state))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (majjic--render-records (list new-record) state)))
      (should (equal (majjic--current-commit-id) "new-commit"))
      (should (equal (majjic--expanded-commit-ids) '("new-commit"))))))

(ert-deftest majjic-test-current-row-ids-are-explicitly-split ()
  "Current-row state should use commit ids, with change ids stored as section metadata."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (should (search-forward "Edit notes in two distant hunks" nil t))
      (beginning-of-line)
      (let* ((section (majjic--current-revision-section))
             (commit-id (majjic--current-commit-id))
             (change-id (oref section change-id)))
        (should (equal commit-id (oref section value)))
        (should (equal change-id (majjic--change-id-for-commit-id commit-id)))
        (should-not (equal commit-id change-id))
        (should (majjic--revision-section-moved-p section (list change-id)))))))

(ert-deftest majjic-test-marks-persist-and-seed-transient-modes ()
  "Persistent marks should survive refresh and seed abandon/rebase entry."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (should (search-forward "Edit notes in two distant hunks" nil t))
      (beginning-of-line)
      (majjic-section-up)
      (let* ((marked-section (majjic--current-revision-section))
             (marked-change-id (oref marked-section change-id))
             (marked-commit-id (oref marked-section value))
             (marked-section-start (marker-position (oref marked-section start))))
        (should (eq (key-binding (kbd "SPC")) #'majjic-space))
        (majjic-space)
        (should (equal majjic--marked-change-ids (list marked-change-id)))
        (should-not (equal (majjic--current-commit-id) marked-commit-id))
        (should (seq-some (lambda (overlay)
                            (and (overlay-start overlay)
                                 (equal (overlay-get overlay 'majjic-mark-change-id)
                                        marked-change-id)
                                 (= (overlay-start overlay) marked-section-start)
                                 (equal (get-text-property 0 'face (overlay-get overlay 'display))
                                        'majjic-marked-row)))
                          majjic--mark-overlays))
        (magit-section-goto marked-section)
        (majjic--sync-mark-overlays)
        (should (seq-some (lambda (overlay)
                            (and (equal (overlay-get overlay 'majjic-mark-change-id)
                                        marked-change-id)
                                 (equal (get-text-property 0 'face (overlay-get overlay 'display))
                                        '(:inherit (majjic-marked-row magit-section-highlight)))))
                          majjic--mark-overlays))
        (majjic-toggle-at-point)
        (majjic-test--wait-for-idle)
        (majjic-section-forward)
        (let ((child-commit-id (majjic--current-commit-id)))
          (majjic-space)
          (should (equal (majjic--current-commit-id) child-commit-id))
          (should-not (member marked-change-id majjic--marked-change-ids))
          (majjic-space)
          (should (equal (majjic--current-commit-id) child-commit-id))
          (should (member marked-change-id majjic--marked-change-ids)))
        (majjic--sync-mark-overlays)
        (should (seq-some (lambda (overlay)
                            (and (equal (overlay-get overlay 'majjic-mark-change-id)
                                        marked-change-id)
                                 (equal (get-text-property 0 'face (overlay-get overlay 'display))
                                        'majjic-marked-row)))
                          majjic--mark-overlays))
        (magit-section-goto marked-section)
        (majjic--log-refresh-sync (majjic--capture-refresh-state))
        (should (equal majjic--marked-change-ids (list marked-change-id)))
        (should (seq-some (lambda (overlay)
                            (equal (overlay-get overlay 'majjic-mark-change-id)
                                   marked-change-id))
                          majjic--mark-overlays))
        (cl-letf* ((prompted nil)
                   (saw-overlay nil)
                   (saw-mark-overlay nil)
                   ((symbol-function 'y-or-n-p)
                    (lambda (_prompt)
                      (setq prompted t)
                      (setq saw-overlay
                            (seq-some (lambda (overlay)
                                        (equal (overlay-get overlay 'majjic-abandon-commit-id)
                                               marked-commit-id))
                                      majjic--abandon-overlays))
                      (setq saw-mark-overlay majjic--mark-overlays)
                      nil)))
          (majjic-abandon-start)
          (should prompted)
          (should saw-overlay)
          (should-not saw-mark-overlay))
        (should-not majjic--abandon-overlays)
        (should-not majjic--abandon-selected-commit-ids)
        (should (equal majjic--marked-change-ids (list marked-change-id)))
        (should (seq-some (lambda (overlay)
                            (equal (overlay-get overlay 'majjic-mark-change-id)
                                   marked-change-id))
                          majjic--mark-overlays))
        (majjic-rebase-start)
        (should (equal (majjic-rebase-state-source-change-id majjic--rebase-state)
                       marked-change-id))
        (goto-char (point-min))
        (let ((top-change-id (oref (majjic--current-revision-section) change-id)))
          (majjic-toggle-mark)
          (should (equal (majjic-rebase-state-source-change-id majjic--rebase-state)
                         (majjic--marked-visible-change-ids)))
          (should (member top-change-id (majjic--rebase-source-change-ids
                                         (majjic-rebase-state-source-change-id majjic--rebase-state)))))
        (majjic-rebase-cancel)
        (should (member marked-change-id majjic--marked-change-ids))
        (majjic-clear-marks)
        (should-not majjic--marked-change-ids)
        (should-not majjic--mark-overlays)))))

(ert-deftest majjic-test-reserved-browser-keybindings-exist ()
  "Top-level browser keybindings should be reserved in the main log."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (should (eq (key-binding "O") #'majjic-op-log-browser))
      (should (eq (key-binding "B") #'majjic-bookmark-browser))
      (should-error (call-interactively #'majjic-op-log-browser) :type 'user-error)
      (should-error (call-interactively #'majjic-bookmark-browser) :type 'user-error))))

(ert-deftest majjic-test-rebase-command-args ()
  "Rebase command args should map target modes to the expected jj flags."
  (should (equal (majjic--rebase-args "src" "dst" 'onto)
                 '("rebase" "--revisions" "src" "--onto" "dst")))
  (should (equal (majjic--rebase-args "src" "dst" 'after)
                 '("rebase" "--revisions" "src" "--insert-after" "dst")))
  (should (equal (majjic--rebase-args "src" "dst" 'before)
                 '("rebase" "--revisions" "src" "--insert-before" "dst")))
  (should (equal (majjic--rebase-args "src" "dst" 'onto 'descendants)
                 '("rebase" "--source" "src" "--onto" "dst")))
  (should (equal (majjic--rebase-args '("src1" "src2") "dst" 'onto)
                 '("rebase" "--revisions" "src1" "--revisions" "src2" "--onto" "dst"))))

(ert-deftest majjic-test-describe-command-args-description-and-binding ()
  "Describe helpers should build args, read descriptions, and bind to `d'."
  (should (equal (majjic--describe-args "commit-1" "hello")
                 '("describe" "-r" "commit-1" "-m" "hello")))
  (let ((majjic--repo-root "/tmp/majjic-test")
        called)
    (cl-letf (((symbol-function 'majjic--call-jj)
               (lambda (_dir &rest args)
                 (setq called args)
                 "current description")))
      (should (equal (majjic--revision-description "commit-1")
                     "current description"))
      (should (equal called '("log" "-r" "commit-1"
                              "--ignore-working-copy" "--no-graph"
                              "--color" "never" "--template" "description")))))
  (with-temp-buffer
    (majjic-log-mode)
    (should (eq (key-binding (kbd "d")) #'majjic-describe))))

(ert-deftest majjic-test-describe-routes-current-revision-through-mutation ()
  "Describe should prompt from the current revision and reselect its change."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic--marked-change-ids '("marked-change"))
          (majjic-rebase-mode nil)
          target-spec
          ran)
      (cl-letf (((symbol-function 'majjic--change-id-for-commit-id)
                 (lambda (commit-id)
                   (should (equal commit-id "commit-1"))
                   "change-1"))
                ((symbol-function 'majjic--revision-description)
                 (lambda (commit-id)
                   (should (equal commit-id "commit-1"))
                   "old description"))
                ((symbol-function 'read-string)
                 (lambda (prompt &optional initial-input &rest _args)
                   (should (equal prompt "Description: "))
                   (should (equal initial-input "old description"))
                   "new description"))
                ((symbol-function 'majjic--commit-id-for-change-id)
                 (lambda (change-id)
                   (should (equal change-id "change-1"))
                   "rewritten-commit"))
                ((symbol-function 'majjic--run-mutation)
                 (lambda (command &rest plist)
                   (setq ran (funcall command))
                   (setq target-spec (plist-get plist :target)))))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "old description"))
           (make-majjic-state)))
        (majjic-describe)
        (should (equal ran '("describe" "-r" "commit-1" "-m" "new description")))
        (should (equal (funcall target-spec) "rewritten-commit"))))))

(ert-deftest majjic-test-describe-rejects-unavailable-states ()
  "Describe should use the normal main-buffer mutation guards."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test"))
      (let ((majjic--mutation-in-progress t))
        (should-error (majjic-describe) :type 'user-error))
      (let ((majjic-rebase-mode t))
        (should-error (majjic-describe) :type 'user-error)))))

(ert-deftest majjic-test-squash-command-args-bindings-and-source-selection ()
  "Squash helpers should build args and mode should seed sources predictably."
  (should (equal (majjic--squash-args "one" "dest")
                 '("squash" "--from" "one" "--into" "dest" "--use-destination-message")))
  (should (equal (majjic--squash-args '("one" "two") "dest")
                 '("squash" "--from" "one" "--from" "two"
                   "--into" "dest" "--use-destination-message")))
  (with-temp-buffer
    (majjic-log-mode)
    (should (eq (key-binding (kbd "s")) #'majjic-squash-start))
    (let ((majjic--repo-root "/tmp/majjic-test"))
      (let ((inhibit-read-only t))
        (majjic--render-records
         (list (make-majjic-revision
                :kind 'revision
                :change-id "change-1"
                :commit-id "commit-1"
                :heading "◆ commit-1 one"
                :summary "one")
               (make-majjic-revision
                :kind 'revision
                :change-id "change-2"
                :commit-id "commit-2"
                :heading "◆ commit-2 two"
                :summary "two")
               (make-majjic-revision
                :kind 'revision
                :change-id "parent-change"
                :commit-id "parent-commit"
                :heading "◆ parent-commit parent"
                :summary "parent"))
         (make-majjic-state)))
      (cl-letf (((symbol-function 'majjic--change-id-for-commit-id)
                 (lambda (commit-id)
                   (should (equal commit-id "commit-1"))
                   "change-1"))
                ((symbol-function 'majjic--commit-id-for-change-id)
                 (lambda (change-id)
                   (pcase change-id
                     ("change-1" "commit-1")
                     (_ nil))))
                ((symbol-function 'majjic--single-parent-commit-id)
                 (lambda (commit-id)
                   (should (equal commit-id "commit-1"))
                   "parent-commit")))
        (majjic-squash-start)
        (should majjic-squash-mode)
        (should (equal (majjic-squash-state-source-change-ids majjic--squash-state)
                       '("change-1")))
        (should (equal (majjic--current-commit-id) "parent-commit"))
        (majjic-squash-cancel))
      (setq majjic--marked-change-ids '("change-2" "change-1"))
      (majjic--sync-mark-overlays)
      (majjic--goto-commit "commit-2")
      (cl-letf (((symbol-function 'majjic--commit-id-for-change-id)
                 (lambda (change-id)
                   (pcase change-id
                     ("change-1" "commit-1")
                     ("change-2" "commit-2")
                     (_ nil))))
                ((symbol-function 'majjic--single-parent-commit-id)
                 (lambda (commit-id)
                   (pcase commit-id
                     ("commit-1" "parent-commit")
                     ("commit-2" "parent-commit")
                     (_ nil)))))
        (majjic-squash-start)
        (should (equal (majjic-squash-state-source-change-ids majjic--squash-state)
                       '("change-1" "change-2")))
        (should (equal (majjic--current-commit-id) "parent-commit"))
        (should (eq (key-binding (kbd "RET")) #'majjic-squash-apply))
        (should (eq (key-binding (kbd "C-g")) #'majjic-squash-cancel))
        (majjic-squash-cancel))
      (should-not majjic-squash-mode)
      (should-not majjic--squash-state)
      (should-not majjic--squash-overlays))))

(ert-deftest majjic-test-squash-start-leaves-point-when-marked-sources-have-different-parents ()
  "Squash mode should not guess a destination for unrelated source parents."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test"))
      (let ((inhibit-read-only t))
        (majjic--render-records
         (list (make-majjic-revision
                :kind 'revision
                :change-id "change-1"
                :commit-id "commit-1"
                :heading "◆ commit-1 one"
                :summary "one")
               (make-majjic-revision
                :kind 'revision
                :change-id "change-2"
                :commit-id "commit-2"
                :heading "◆ commit-2 two"
                :summary "two")
               (make-majjic-revision
                :kind 'revision
                :change-id "parent-a"
                :commit-id "parent-a-commit"
                :heading "◆ parent-a-commit parent a"
                :summary "parent a")
               (make-majjic-revision
                :kind 'revision
                :change-id "parent-b"
                :commit-id "parent-b-commit"
                :heading "◆ parent-b-commit parent b"
                :summary "parent b"))
         (make-majjic-state :marked-change-ids '("change-1" "change-2"))))
      (majjic--goto-commit "commit-2")
      (cl-letf (((symbol-function 'majjic--commit-id-for-change-id)
                 (lambda (change-id)
                   (pcase change-id
                     ("change-1" "commit-1")
                     ("change-2" "commit-2")
                     (_ nil))))
                ((symbol-function 'majjic--single-parent-commit-id)
                 (lambda (commit-id)
                   (pcase commit-id
                     ("commit-1" "parent-a-commit")
                     ("commit-2" "parent-b-commit")
                     (_ nil)))))
        (majjic-squash-start)
        (should (equal (majjic--current-commit-id) "commit-2"))
        (majjic-squash-cancel)))))

(ert-deftest majjic-test-squash-mode-preview-and-apply-routing ()
  "Squash mode should preview sources and route apply through mutation."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          target-spec
          after-success
          ran)
      (let ((inhibit-read-only t))
        (majjic--render-records
         (list (make-majjic-revision
                :kind 'revision
                :change-id "source-1"
                :commit-id "commit-1"
                :heading "◆ commit-1 source one"
                :summary "source one")
               (make-majjic-revision
                :kind 'revision
                :change-id "source-2"
                :commit-id "commit-2"
                :heading "◆ commit-2 source two"
                :summary "source two")
               (make-majjic-revision
                :kind 'revision
                :change-id "dest"
                :commit-id "commit-3"
                :heading "◆ commit-3 destination"
                :summary "destination"))
         (make-majjic-state :marked-change-ids '("source-1" "source-2"))))
      (majjic-squash-start)
      (should (seq-some (lambda (overlay)
                          (let ((text (overlay-get overlay 'before-string)))
                            (and (stringp text)
                                 (string-match-p "<< squash >>" text))))
                        majjic--squash-overlays))
      (majjic--goto-commit "commit-3")
      (majjic--sync-squash-overlays)
      (should (seq-some (lambda (overlay)
                          (let ((text (overlay-get overlay 'before-string)))
                            (and (stringp text)
                                 (string-match-p "<< into >>" text)
                                 (string-match-p "squash 2 revisions" text)
                                 (string-match-p "dest" text))))
                        majjic--squash-overlays))
      (cl-letf (((symbol-function 'majjic--change-id-for-commit-id)
                 (lambda (commit-id)
                   (pcase commit-id
                     ("commit-1" "source-1")
                     ("commit-2" "source-2")
                     ("commit-3" "dest"))))
                ((symbol-function 'majjic--selection-fallbacks)
                 (lambda (_commit-id)
                   nil))
                ((symbol-function 'majjic--commit-id-for-change-id)
                 (lambda (change-id)
                   (should (equal change-id "dest"))
                   "rewritten-dest"))
                ((symbol-function 'majjic--run-mutation)
                 (lambda (command &rest plist)
                   (setq ran (funcall command))
                   (setq target-spec (plist-get plist :target))
                   (setq after-success (plist-get plist :after-success)))))
        (majjic-squash-apply)
        (should (equal ran '("squash" "--from" "source-1" "--from" "source-2"
                             "--into" "commit-3" "--use-destination-message")))
        (funcall after-success)
        (should-not majjic-squash-mode)
        (should (equal (funcall target-spec) "rewritten-dest"))))))

(ert-deftest majjic-test-squash-rejects-unavailable-states ()
  "Squash should use normal guards and reject invalid destinations."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test"))
      (let ((inhibit-read-only t))
        (majjic--render-records
         (list (make-majjic-revision
                :kind 'revision
                :change-id "source"
                :commit-id "commit-1"
                :heading "◆ commit-1 source"
                :summary "source"))
         (make-majjic-state)))
      (let ((majjic--mutation-in-progress t))
        (should-error (majjic-squash-start) :type 'user-error))
      (let ((majjic-rebase-mode t))
        (should-error (majjic-squash-start) :type 'user-error))
      (setq majjic--squash-state
            (make-majjic-squash-state :source-change-ids '("source")))
      (majjic-squash-mode 1)
      (cl-letf (((symbol-function 'majjic--change-id-for-commit-id)
                 (lambda (_commit-id) "source"))
                ((symbol-function 'majjic--run-mutation)
                 (lambda (&rest _args)
                   (error "should not mutate"))))
        (should-error (majjic-squash-apply) :type 'user-error))
      (should majjic-squash-mode)
      (should-error (majjic-toggle-mark) :type 'user-error)
      (should-error (majjic-rebase-start) :type 'user-error)
      (majjic-squash-cancel))))

(ert-deftest majjic-test-git-remote-command-args-and-bindings ()
  "Git remote helpers should build expected args and live under a `G' prefix."
  (should (equal (majjic--git-fetch-args)
                 '("git" "fetch")))
  (should (equal (majjic--git-fetch-args t)
                 '("git" "fetch" "--tracked")))
  (should (equal (majjic--git-push-change-args '("one"))
                 '("git" "push" "--change" "one")))
  (should (equal (majjic--git-push-change-args '("one" "two") t)
                 '("git" "push" "--change" "one" "--change" "two" "--dry-run")))
  (should (equal (majjic--git-push-revision-args '("one"))
                 '("git" "push" "--revision" "one")))
  (should (equal (majjic--git-push-revision-args '("one" "two") t)
                 '("git" "push" "--revision" "one" "--revision" "two" "--dry-run")))
  (with-temp-buffer
    (majjic-log-mode)
    (should (eq (key-binding (kbd "G f f")) #'majjic-git-fetch))
    (should (eq (key-binding (kbd "G f t")) #'majjic-git-fetch-tracked))
    (should (eq (key-binding (kbd "G p c")) #'majjic-git-push-change))
    (should (eq (key-binding (kbd "G p p")) #'majjic-git-push))
    (should (eq (key-binding (kbd "p")) #'majjic-section-backward))))

(ert-deftest majjic-test-git-push-confirmation-flow ()
  "Push should preview, honor decline, and execute on confirmation."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic-rebase-mode nil)
          (previewed nil)
          (preview-args nil)
          (ran nil))
      (cl-letf* (((symbol-function 'majjic--marked-visible-commit-ids)
                  (lambda () '("c1" "c2")))
                 ((symbol-function 'majjic--git-push-preview-result-async)
                  (lambda (commit-ids args-function callback)
                    (setq previewed commit-ids)
                    (setq preview-args (funcall args-function commit-ids t))
                    (funcall callback 0 "dry-run")))
                 ((symbol-function 'majjic--confirm-preview)
                  (lambda (_action _preview _prompt) nil))
                 ((symbol-function 'majjic--run-mutation)
                  (lambda (&rest _args)
                    (setq ran t))))
        (majjic-git-push-change)
        (should (equal previewed '("c1" "c2")))
        (should (equal preview-args
                       '("git" "push" "--change" "c1" "--change" "c2" "--dry-run")))
        (should-not ran))
      (setq previewed nil
            preview-args nil
            ran nil)
      (cl-letf* (((symbol-function 'majjic--marked-visible-commit-ids)
                  (lambda () nil))
                 ((symbol-function 'majjic--require-current-commit-id)
                  (lambda () "current"))
                 ((symbol-function 'majjic--git-push-preview-result-async)
                  (lambda (commit-ids args-function callback)
                    (setq previewed commit-ids)
                    (setq preview-args (funcall args-function commit-ids t))
                    (funcall callback 0 "dry-run")))
                 ((symbol-function 'majjic--confirm-preview)
                  (lambda (_action _preview _prompt) t))
                 ((symbol-function 'majjic--run-mutation)
                  (lambda (command &rest _args)
                    (setq ran t)
                    (should (equal (funcall command)
                                   '("git" "push" "--change" "current"))))))
        (majjic-git-push-change)
        (should (equal previewed '("current")))
        (should (equal preview-args
                       '("git" "push" "--change" "current" "--dry-run")))
        (should ran))
      (setq previewed nil
            preview-args nil
            ran nil)
      (cl-letf* (((symbol-function 'majjic--marked-visible-commit-ids)
                  (lambda () nil))
                 ((symbol-function 'majjic--require-current-commit-id)
                  (lambda () "current"))
                 ((symbol-function 'majjic--git-push-preview-result-async)
                  (lambda (commit-ids args-function callback)
                    (setq previewed commit-ids)
                    (setq preview-args (funcall args-function commit-ids t))
                    (funcall callback 0 "dry-run")))
                 ((symbol-function 'majjic--confirm-preview)
                  (lambda (_action _preview _prompt) t))
                 ((symbol-function 'majjic--run-mutation)
                  (lambda (command &rest _args)
                    (setq ran t)
                    (should (equal (funcall command)
                                   '("git" "push" "--revision" "current"))))))
        (majjic-git-push)
        (should (equal previewed '("current")))
        (should (equal preview-args
                       '("git" "push" "--revision" "current" "--dry-run")))
        (should ran))
      (let ((failed-preview nil))
        (cl-letf* (((symbol-function 'majjic--marked-visible-commit-ids)
                    (lambda () '("bad")))
                   ((symbol-function 'majjic--git-push-preview-result-async)
                    (lambda (_commit-ids _args-function callback)
                      (funcall callback 1 "\e[31mError\e[0m")))
                   ((symbol-function 'majjic--show-preview)
                    (lambda (_action preview _prompt)
                      (setq failed-preview preview))))
          (majjic-git-push)
          (should (equal failed-preview "\e[31mError\e[0m"))))
      (setq majjic-rebase-mode t)
      (should-error (majjic-git-push-change) :type 'user-error)
      (should-error (majjic-git-push) :type 'user-error))))

(ert-deftest majjic-test-git-push-preview-blocks-overlapping-operations ()
  "The async push preview should block other repository operations."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic-rebase-mode nil))
      (unwind-protect
          (cl-letf* (((symbol-function 'majjic--marked-visible-commit-ids)
                      (lambda () '("c1")))
                     ((symbol-function 'majjic--git-push-preview-result-async)
                      (lambda (_commit-ids _args-function _callback)
                        'preview-process))
                     ((symbol-function 'majjic--confirm-preview)
                      (lambda (&rest _args)
                        (error "confirmation should wait for preview callback"))))
            (majjic-git-push)
            (should majjic--preview-in-progress)
            (should (eq majjic--preview-process 'preview-process))
            (should-error (majjic-git-fetch) :type 'user-error))
        (setq majjic--preview-in-progress nil)
        (setq majjic--preview-process nil)
        (majjic--set-operation-status nil)))))

(ert-deftest majjic-test-git-fetch-routes-through-mutation ()
  "Fetch should use the shared mutation path and call `jj git fetch'."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic-rebase-mode nil)
          (ran nil))
      (cl-letf (((symbol-function 'majjic--run-mutation)
                 (lambda (command &rest _args)
                   (setq ran t)
                   (should (equal (funcall command) '("git" "fetch"))))))
        (majjic-git-fetch)
        (should ran)))
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic-rebase-mode nil)
          (ran nil))
      (cl-letf (((symbol-function 'majjic--run-mutation)
                 (lambda (command &rest _args)
                   (setq ran t)
                   (should (equal (funcall command) '("git" "fetch" "--tracked"))))))
        (majjic-git-fetch-tracked)
        (should ran)))
    (let ((majjic-rebase-mode t))
      (should-error (majjic-git-fetch) :type 'user-error))))

(ert-deftest majjic-test-async-refresh-drops-stale-callbacks ()
  "Only the latest async refresh callback should update the buffer."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          requests
          rendered)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest args)
                   (push (cons args callback) requests)
                   (list 'process (length requests))))
                ((symbol-function 'majjic--render-records)
                 (lambda (records _state)
                   (setq rendered records)))
                ((symbol-function 'majjic--parse-log-output)
                 (lambda (output)
                   output)))
        (majjic-log-refresh)
        (majjic-log-refresh)
        (should (= majjic--process-generation 2))
        (should (equal (caar requests) (majjic--refresh-snapshot-args)))
        (funcall (cdr (nth 1 requests)) 0 "stale snapshot" "")
        (should (= (length requests) 2))
        (should-not rendered)
        (funcall (cdar requests) 0 "fresh snapshot" "")
        (should (= (length requests) 3))
        (should (equal (caar requests) (majjic--log-args)))
        (funcall (cdar requests) 0 "fresh" "")
        (should (equal rendered "fresh"))
        (should-not mode-line-process)))))

(ert-deftest majjic-test-async-refresh-renders-snapshot-failure-and-skips-log ()
  "A failed snapshot preflight should stop refresh before the read-only log."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          requests)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest args)
                   (push (cons args callback) requests)
                   (list 'process (length requests)))))
        (majjic-log-refresh)
        (funcall (cdar requests) 1 "" "Error: The working copy is stale")
        (should (= (length requests) 1))
        (should (string-match-p "working copy is stale"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (should-not mode-line-process)
        (should-not majjic--mutation-in-progress)
        (should-not majjic--refresh-process)))))

(ert-deftest majjic-test-sync-refresh-snapshots-before-log-read ()
  "Synchronous refresh should snapshot before the read-only log."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          calls)
      (cl-letf (((symbol-function 'majjic--call-jj-capture)
                 (lambda (_dir &rest args)
                   (push args calls)
                   (list 0 "" "")))
                ((symbol-function 'majjic--read-log-records)
                 (lambda ()
                   (push :log-read calls)
                   nil)))
        (majjic--log-refresh-sync (make-majjic-state))
        (should (equal (nreverse calls)
                       (list (majjic--refresh-snapshot-args)
                             :log-read)))))))

(ert-deftest majjic-test-sync-refresh-renders-snapshot-failure-and-skips-log ()
  "A failed synchronous snapshot should stop refresh before the read-only log."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          log-read)
      (cl-letf (((symbol-function 'majjic--call-jj-capture)
                 (lambda (_dir &rest _args)
                   (list 1 "" "Error: The working copy is stale")))
                ((symbol-function 'majjic--read-log-records)
                 (lambda ()
                   (setq log-read t)
                   nil)))
        (majjic--log-refresh-sync (make-majjic-state))
        (should-not log-read)
        (should (string-match-p "working copy is stale"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

(ert-deftest majjic-test-async-refresh-snapshots-working-copy-and-preserves-view ()
  "Refresh should snapshot disk edits and keep the rewritten `@' selected/expanded."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-refresh-snapshot-" t))
         (buffer-name (majjic--log-buffer-name repo))
         (default-directory (file-name-as-directory repo)))
    (unwind-protect
        (progn
          (majjic-test--jj repo "git" "init" ".")
          (majjic-test--configure-jj-repo repo)
          (majjic-test--jj repo "describe" "-m" "snapshot target")
          (majjic-test--write-repo-file repo "tracked.txt" "one\n")
          (majjic-test--jj repo "status")
          (majjic)
          (with-current-buffer buffer-name
            (let* ((old-commit-id (majjic--current-commit-id))
                   (change-id (oref (majjic--current-revision-section) change-id)))
              (majjic-toggle-at-point)
              (majjic-test--wait-for-idle)
              (should (equal (majjic--expanded-commit-ids) (list old-commit-id)))
              (majjic-test--write-repo-file repo "tracked.txt" "one\ntwo\n")
              (majjic-log-refresh)
              (majjic-test--wait-for-idle)
              (let ((new-commit-id (majjic--current-commit-id)))
                (should-not (equal new-commit-id old-commit-id))
                (should (equal (oref (majjic--current-revision-section) change-id)
                               change-id))
                (should (equal (majjic--expanded-commit-ids) (list new-commit-id))))
              (majjic-test--wait-for
               (lambda ()
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "tracked.txt" nil t)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest majjic-test-async-refresh-surfaces-stale-workspace-without-updating-it ()
  "Refresh should surface stale workspaces and not run workspace recovery."
  (skip-unless (executable-find "jj"))
  (let* ((base (make-temp-file "majjic-refresh-stale-" t))
         (main (expand-file-name "main" base))
         (other (expand-file-name "other" base))
         (buffer-name (majjic--log-buffer-name other))
         (local-only (expand-file-name "local-only.txt" other)))
    (make-directory main)
    (unwind-protect
        (let ((default-directory (file-name-as-directory other)))
          (majjic-test--jj main "git" "init" ".")
          (majjic-test--configure-jj-repo main)
          (majjic-test--jj main "describe" "-m" "main workspace")
          (majjic-test--write-repo-file main "main.txt" "one\n")
          (majjic-test--jj main "status")
          (majjic-test--jj main "workspace" "add" "../other")
          (majjic-test--jj main "rebase" "-r" "other@" "-d" "default@")
          (majjic-test--jj other "workspace" "update-stale")
          (majjic-test--write-repo-file other "other.txt" "other\n")
          (majjic-test--jj other "status")
          (majjic)
          (with-current-buffer buffer-name
            (should (majjic--current-commit-id)))
          (majjic-test--write-repo-file main "main.txt" "one\ntwo\n")
          (majjic-test--jj main "status")
          (majjic-test--write-repo-file other "local-only.txt" "do not delete\n")
          (with-current-buffer buffer-name
            (majjic-log-refresh)
            (majjic-test--wait-for-idle)
            (should (string-match-p "working copy is stale"
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))
          (should (file-exists-p local-only)))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest majjic-test-async-mutation-serializes-in-flight-commands ()
  "A second mutation should be rejected while the first async process is live."
  (with-temp-buffer
    (majjic-log-mode)
    (let* ((majjic--repo-root "/tmp/majjic-test")
           (process (make-process
                     :name "majjic-test-sleep"
                     :buffer nil
                     :command '("sleep" "1")
                     :noquery t)))
      (unwind-protect
          (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                     (lambda (_dir _callback &rest _args)
                       process)))
            (majjic--run-mutation '("first"))
            (should majjic--mutation-in-progress)
            (should (eq majjic--mutation-process process))
            (should-error (majjic--run-mutation '("second")) :type 'user-error))
        (when (process-live-p process)
          (delete-process process))
        (setq majjic--mutation-process nil)
        (setq majjic--mutation-in-progress nil)
        (majjic--set-operation-status nil)))))

(ert-deftest majjic-test-async-summary-expansion-loads-file-rows ()
  "Lazy summary expansion should populate file rows asynchronously."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          requests)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest args)
                   (push (cons args callback) requests)
                   'summary-process)))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (should (= (length requests) 1))
        (should (member "--ignore-working-copy" (caar requests)))
        (funcall (cdar requests) 0 "M file.txt\n" "")
        (let* ((revision (majjic--current-revision-section))
               (summary (majjic--summary-child revision))
               (file (car (oref summary children))))
          (should (object-of-class-p file 'majjic-file-section))
          (should (equal (oref file path) "file.txt"))
          (should-not (oref summary load-process)))))))

(ert-deftest majjic-test-async-summary-expansion-skips-oversized-summary ()
  "Oversized lazy summaries should not render thousands of file sections."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          (majjic-file-summary-limit 2)
          callback)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir cb &rest _args)
                   (setq callback cb)
                   'summary-process)))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (funcall callback 0 "M one.txt\nM two.txt\nM three.txt\n" "")
        (let* ((revision (majjic--current-revision-section))
               (summary (majjic--summary-child revision))
               (body (majjic-test--section-body-string summary)))
          (should-not (oref summary children))
          (should (string-match-p "3 file changes" body))
          (should (string-match-p "limit is 2" body)))))))

(ert-deftest majjic-test-async-summary-expansion-skips-fast-loading-placeholder ()
  "Fast summary loads should render results without flashing a loading placeholder."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          callback
          timer-callback)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir cb &rest _args)
                   (setq callback cb)
                   'summary-process))
                ((symbol-function 'run-at-time)
                 (lambda (secs repeat function &rest _args)
                   (should (= secs majjic-section-loading-delay))
                   (should-not repeat)
                   (setq timer-callback function)
                   'summary-timer)))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (let* ((revision (majjic--current-revision-section))
               (summary (majjic--summary-child revision)))
          (should (equal (majjic-test--section-body-string summary) ""))
          (should (eq (oref summary load-timer) 'summary-timer))
          (funcall callback 0 "M file.txt\n" "")
          (should-not (oref summary load-timer))
          (should-not (string-match-p "(loading\\.\\.\\.)"
                                      (majjic-test--section-body-string summary)))
          (funcall timer-callback)
          (should (= (length (oref summary children)) 1))
          (should-not (string-match-p "(loading\\.\\.\\.)"
                                      (majjic-test--section-body-string summary))))))))

(ert-deftest majjic-test-async-summary-expansion-shows-loading-placeholder-after-delay ()
  "Slow summary loads should show a delayed loading placeholder before results."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          callback
          timer-callback)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir cb &rest _args)
                   (setq callback cb)
                   'summary-process))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat function &rest _args)
                   (setq timer-callback function)
                   'summary-timer)))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (let* ((revision (majjic--current-revision-section))
               (summary (majjic--summary-child revision)))
          (funcall timer-callback)
          (should-not (oref summary load-timer))
          (should (string-match-p "(loading\\.\\.\\.)"
                                  (majjic-test--section-body-string summary)))
          (funcall callback 0 "M file.txt\n" "")
          (should (= (length (oref summary children)) 1))
          (should-not (string-match-p "(loading\\.\\.\\.)"
                                      (majjic-test--section-body-string summary))))))))

(ert-deftest majjic-test-async-file-diff-expansion-loads-hunks ()
  "Lazy file expansion should populate diff hunks asynchronously."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          requests)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest args)
                   (push (cons args callback) requests)
                   (car args))))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (should (member "--ignore-working-copy" (caar requests)))
        (funcall (cdar requests) 0 "M file.txt\n" "")
        (majjic-section-forward)
        (majjic-toggle-at-point)
        (should (member "--ignore-working-copy" (caar requests)))
        (funcall (cdar requests) 0 "@@ -1 +1 @@\n-old\n+new\n" "")
        (let* ((file (majjic--current-file-section))
               (hunk (car (oref file children))))
          (should (object-of-class-p hunk 'majjic-hunk-section))
          (should (equal (oref hunk body-lines) '("-old" "+new")))
          (should-not (oref file load-process)))))))

(ert-deftest majjic-test-async-section-load-drops-stale-callbacks ()
  "Section load callbacks from a previous render should be ignored."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          callbacks
          (records (list (make-majjic-revision
                          :kind 'revision
                          :change-id "change-1"
                          :commit-id "commit-1"
                          :heading "◆ commit-1 test"
                          :summary "M file.txt"))))
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir cb &rest _args)
                   (push cb callbacks)
                   'summary-process)))
        (let ((inhibit-read-only t))
          (majjic--render-records records (make-majjic-state)))
        (goto-char (point-min))
        (let* ((old-revision (majjic--current-revision-section))
               (old-summary (majjic--summary-child old-revision)))
          (majjic-toggle-at-point)
          (let ((inhibit-read-only t))
            (majjic--render-records records (make-majjic-state)))
          (funcall (car (last callbacks)) 0 "M stale.txt\n" "")
          (should-not (oref old-summary children))
          (should-not (oref (majjic--summary-child (majjic--current-revision-section)) children)))))))

(ert-deftest majjic-test-async-section-load-drops-stale-loading-timers ()
  "Loading timers from a previous render should be ignored."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          timer-callbacks
          (records (list (make-majjic-revision
                          :kind 'revision
                          :change-id "change-1"
                          :commit-id "commit-1"
                          :heading "◆ commit-1 test"
                          :summary "M file.txt"))))
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir _cb &rest _args)
                   'summary-process))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat function &rest _args)
                   (push function timer-callbacks)
                   (list 'summary-timer (length timer-callbacks)))))
        (let ((inhibit-read-only t))
          (majjic--render-records records (make-majjic-state)))
        (goto-char (point-min))
        (let* ((old-revision (majjic--current-revision-section))
               (old-summary (majjic--summary-child old-revision)))
          (majjic-toggle-at-point)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (majjic--render-records records (make-majjic-state)))
          (should (= (length timer-callbacks) 2))
          (funcall (car (last timer-callbacks)))
          (should (equal (majjic-test--section-body-string old-summary) ""))
          (should (equal (majjic-test--section-body-string
                          (majjic--summary-child (majjic--current-revision-section)))
                         "")))))))

(ert-deftest majjic-test-async-section-load-restarts-after-close ()
  "Closing a loading section should cancel the old request and restart on reopen."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          callbacks
          processes
          timer-callbacks
          timers)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest _args)
                   (let ((process (make-process
                                   :name "majjic-test-section-load"
                                   :buffer nil
                                   :command '("sleep" "5")
                                   :noquery t)))
                     (push callback callbacks)
                     (push process processes)
                     process)))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat function &rest _args)
                   (let ((timer (list 'summary-timer (1+ (length timers)))))
                     (push function timer-callbacks)
                     (push timer timers)
                     timer))))
        (unwind-protect
            (progn
              (let ((inhibit-read-only t))
                (majjic--render-records
                 (list (make-majjic-revision
                        :kind 'revision
                        :change-id "change-1"
                        :commit-id "commit-1"
                        :heading "◆ commit-1 test"
                        :summary "M file.txt"))
                 (make-majjic-state)))
              (goto-char (point-min))
              (majjic-toggle-at-point)
              (let* ((revision (majjic--current-revision-section))
                     (summary (majjic--summary-child revision))
                     (first-process (car processes))
                     (first-timer (car timers))
                     (first-timer-callback (car timer-callbacks)))
                (should (process-live-p first-process))
                (should (equal (oref summary load-timer) first-timer))
                (majjic-toggle-at-point)
                (should-not (process-live-p first-process))
                (should-not (oref summary load-timer))
                (should (oref summary hidden))
                (funcall first-timer-callback)
                (should (equal (majjic-test--section-body-string summary) ""))
                (majjic-toggle-at-point)
                (should (= (length callbacks) 2))
                (should (= (length timers) 2))
                (should (equal (oref summary load-timer) (car timers)))
                (funcall (car (last callbacks)) 0 "M stale.txt\n" "")
                (should (equal (oref summary load-timer) (car timers)))))
          (dolist (process processes)
            (when (process-live-p process)
              (delete-process process))))))))

(ert-deftest majjic-test-async-mutation-recaptures-expanded-state ()
  "Expansions made during a mutation should be preserved by the completion refresh."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          mutation-callback
          refresh-state)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir callback &rest args)
                   (if (equal args '("describe"))
                       (progn
                         (setq mutation-callback callback)
                         'mutation-process)
                     'summary-process)))
                ((symbol-function 'majjic--log-refresh-async)
                 (lambda (state &optional _generation)
                   (setq refresh-state state))))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (majjic--run-mutation '("describe"))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (funcall mutation-callback 0 "" "")
        (should (equal (majjic-state-expanded-commit-ids refresh-state)
                       '("commit-1")))))))

(ert-deftest majjic-test-in-flight-mutation-blocks-main-buffer-commands ()
  "Mutations should block state edits, but still allow unloaded lazy reads."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (goto-char (point-min))
      (let ((majjic--mutation-in-progress t))
        (should-error (majjic-toggle-mark) :type 'user-error)
        (should-error (majjic-clear-marks) :type 'user-error)
        (should-error (majjic-log-refresh) :type 'user-error)
        (should-error (majjic-rebase-start) :type 'user-error)
        (majjic-toggle-at-point)
        (majjic-section-forward))
      (majjic-test--wait-for-idle)
      (setq majjic--mutation-in-progress nil)
      (goto-char (point-min))
      (majjic-toggle-at-point)
      (let ((majjic--mutation-in-progress t))
        (majjic-toggle-at-point)
        (majjic-toggle-at-point)))))

(ert-deftest majjic-test-in-flight-mutation-allows-snapshot-visits ()
  "Snapshot-style file visits should still work while a mutation is in flight."
  (with-temp-buffer
    (majjic-log-mode)
    (let ((majjic--repo-root "/tmp/majjic-test")
          callback
          called)
      (cl-letf (((symbol-function 'majjic--call-jj-capture-async)
                 (lambda (_dir cb &rest _args)
                   (setq callback cb)
                   'summary-process))
                ((symbol-function 'majjic--visit-file-new-side)
                 (lambda (commit-id path &optional _line _column)
                   (setq called (list commit-id path)))))
        (let ((inhibit-read-only t))
          (majjic--render-records
           (list (make-majjic-revision
                  :kind 'revision
                  :change-id "change-1"
                  :commit-id "commit-1"
                  :heading "◆ commit-1 test"
                  :summary "M file.txt"))
           (make-majjic-state)))
        (goto-char (point-min))
        (majjic-toggle-at-point)
        (funcall callback 0 "M file.txt\n" "")
        (majjic-section-forward)
        (let ((majjic--mutation-in-progress t))
          (majjic-visit-file))
        (should (equal called '("commit-1" "file.txt")))))))

(ert-deftest majjic-test-new-uses-marked-revisions-when-present ()
  "`majjic-new' should use visible marked revisions as parents when present."
  (skip-unless (file-directory-p "/Users/andyphan/code/majjic-demo/.jj"))
  (let ((default-directory "/Users/andyphan/code/majjic-demo/"))
    (majjic-test--kill-demo-buffer)
    (majjic)
    (with-current-buffer "majjic: majjic-demo"
      (let* ((revisions (seq-filter (lambda (child)
                                      (object-of-class-p child 'majjic-revision-section))
                                    (oref magit-root-section children)))
             (parents (mapcar (lambda (revision) (oref revision value))
                              (seq-take revisions 2)))
             (majjic--marked-change-ids (mapcar (lambda (revision) (oref revision change-id))
                                                (seq-take revisions 2)))
             (called nil))
        (cl-letf (((symbol-function 'majjic--run-mutation)
                   (lambda (command &rest _args)
                     (setq called (funcall command)))))
          (majjic-new))
        (should (equal called (cons "new" parents)))))))

(ert-deftest majjic-test-undo-redo-command-args ()
  "Undo and redo helpers should return the expected jj command shapes."
  (should (equal (majjic--undo-args) '("undo")))
  (should (equal (majjic--redo-args) '("redo")))
  (let ((majjic--repo-root "/tmp")
        (called nil))
    (cl-letf (((symbol-function 'majjic--call-jj)
               (lambda (_dir &rest args)
                 (setq called args)
                 "op-summary\n")))
      (should (equal (majjic--op-log-preview) "op-summary"))
      (should (equal called '("--at-op" "@" "--ignore-working-copy"
                              "op" "log" "-n" "1" "--color" "always"
                              "--no-pager"))))))

(ert-deftest majjic-test-op-preview-buffer-renders-ansi-colors ()
  "The temporary operation preview buffer should render ANSI faces, not escapes."
  (let ((buffer (get-buffer-create "*majjic test preview*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'majjic--op-log-preview)
                     (lambda ()
                       "\e[31mred op\e[0m\nplain")))
            (majjic--prepare-op-preview-buffer buffer (majjic--op-log-preview)))
          (with-current-buffer buffer
            (should (equal (buffer-string) "red op\nplain\n"))
            (should-not (string-match-p "\e\\[" (buffer-string)))
            (should (seq-some (lambda (overlay)
                                (overlay-get overlay 'face))
                              (overlays-at (point-min))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest majjic-test-confirm-latest-operation-peek-cleans-up-on-decline ()
  "Declining confirmation should close and kill the temporary side-window peek."
  (save-window-excursion
    (let ((selected (selected-window))
          (display-called nil)
          (original-display (symbol-function 'display-buffer-in-side-window)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'majjic--effective-undo-operation-preview)
                       (lambda ()
                         "@  abc user 1 minute ago\n│  new empty commit"))
                      ((symbol-function 'display-buffer-in-side-window)
                       (lambda (buffer alist)
                         (setq display-called (list buffer alist))
                         (funcall original-display buffer alist)))
                      ((symbol-function 'y-or-n-p)
                       (lambda (_prompt)
                         (should (equal (selected-window) selected))
                         (should (get-buffer-window "*majjic undo preview*"))
                         nil)))
              (should-not (majjic--confirm-latest-operation "undo")))
            (should display-called)
            (should (equal (alist-get 'side (cadr display-called)) 'bottom))
            (should-not (get-buffer "*majjic undo preview*")))
        (when-let* ((buffer (get-buffer "*majjic undo preview*")))
          (kill-buffer buffer))))))

(ert-deftest majjic-test-confirm-latest-operation-peek-cleans-up-on-quit ()
  "`C-g' during confirmation should also clean up the temporary peek."
  (cl-letf (((symbol-function 'majjic--effective-redo-operation-preview)
             (lambda ()
               "@  abc user 1 minute ago\n│  new empty commit"))
            ((symbol-function 'y-or-n-p)
             (lambda (_prompt)
               (keyboard-quit))))
    (let ((quit-seen nil))
      (save-window-excursion
        (condition-case nil
            (majjic--confirm-latest-operation "redo")
          (quit (setq quit-seen t))))
      (should quit-seen))
    (should-not (get-buffer "*majjic redo preview*"))))

(ert-deftest majjic-test-confirm-latest-operation-fetches-preview-in-log-buffer ()
  "Fetching op-log preview should happen before switching to the temporary buffer."
  (with-temp-buffer
    (let ((majjic--repo-root "/repo")
          (called-root nil))
      (cl-letf (((symbol-function 'majjic--effective-undo-operation-preview)
                 (lambda ()
                   (setq called-root majjic--repo-root)
                   "latest op"))
                ((symbol-function 'display-buffer-in-side-window)
                 (lambda (&rest _args)
                   (selected-window)))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil)))
        (should-not (majjic--confirm-latest-operation "undo"))
        (should (equal called-root "/repo"))))))

(ert-deftest majjic-test-prompts-for-directory-outside-repo ()
  "Opening `majjic' outside a repo should prompt for a directory first."
  (let* ((start-dir (file-name-as-directory (make-temp-file "majjic-start-" t)))
         (picked-dir (file-name-as-directory (make-temp-file "majjic-picked-" t)))
         (buffer-name (majjic--log-buffer-name picked-dir))
         (prompted nil)
         (opened-root nil)
         (opened-default-directory nil))
    (unwind-protect
        (let ((default-directory start-dir))
          (cl-letf (((symbol-function 'majjic--locate-root)
                     (lambda (dir)
                       (cond
                        ((equal dir start-dir) nil)
                        ((equal dir picked-dir) picked-dir))))
                    ((symbol-function 'read-directory-name)
                     (lambda (&rest _args)
                       (setq prompted t)
                       picked-dir))
                    ((symbol-function 'majjic--log-refresh-sync)
                     (lambda (_state)
                       (setq opened-root majjic--repo-root)
                       (setq opened-default-directory default-directory)))
                    ((symbol-function 'pop-to-buffer) #'ignore))
            (majjic))
          (should prompted)
          (should (equal opened-root picked-dir))
          (should (equal opened-default-directory picked-dir))
          (should (get-buffer buffer-name)))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (delete-directory start-dir t)
      (delete-directory picked-dir t))))

(ert-deftest majjic-test-effective-undo-preview-skips-synthetic-undo-tip ()
  "A second undo preview should describe the underlying operation, not the top undo op."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-undo-preview-" t))
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
        (insert "one\n"))
      (jj "describe" "-m" "one")
      (jj "new")
      (jj "describe" "-m" "two")
      (jj "undo" "--quiet")
      (let* ((majjic--repo-root repo)
             (preview (majjic--effective-undo-operation-preview)))
        (should (string-match-p "Will undo operation:" preview))
        (should (string-match-p "args: jj new" preview))
        (should-not (string-match-p "Will restore to operation:" preview))
        (should-not (string-match-p "Will undo operation:\n@  [^\n]+\n│  undo: restore to operation" preview))))))

(ert-deftest majjic-test-effective-redo-preview-shows-underlying-operation ()
  "A first redo after two undos should describe the real operation being redone."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-redo-preview-" t))
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
        (insert "one\n"))
      (jj "describe" "-m" "one")
      (jj "new")
      (jj "describe" "-m" "two")
      (jj "new")
      (jj "describe" "-m" "three")
      (jj "undo" "--quiet")
      (jj "undo" "--quiet")
      (let* ((majjic--repo-root repo)
             (preview (majjic--effective-redo-operation-preview)))
        (should (string-match-p "Will redo operation:" preview))
        (should (string-match-p "args: jj new" preview))
        (should-not (string-match-p "Will restore to operation:" preview))
        (should-not (string-match-p "\n│  \\(?:undo\\|redo\\): restore to operation" preview))))))

(ert-deftest majjic-test-undo-cancel-does-not-mutate ()
  "Declining undo confirmation should skip the mutation runner."
  (let ((majjic--repo-root "/tmp")
        (called nil))
    (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
               (lambda (_action) nil))
              ((symbol-function 'majjic--run-mutation)
               (lambda (&rest _args) (setq called t))))
      (majjic-undo))
    (should-not called)))

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
      (let ((source (majjic--current-commit-id)))
        (majjic-rebase-start)
        (should majjic-rebase-mode)
        (should (eq (majjic-rebase-state-source-mode majjic--rebase-state) 'revision))
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
        (goto-char (point-min))
        (should (search-forward "Remove fallback name assignment" nil t))
        (beginning-of-line)
        (majjic--sync-rebase-overlays)
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
               (onto-text (overlay-get onto-overlay 'before-string))
               (target-section (majjic--current-revision-section))
               (target-change-id (oref target-section change-id))
               (target-commit-id (oref target-section value)))
          (should (string-match-p "<< onto >>" onto-text))
          (should (equal (get-text-property 0 'face onto-text) 'default))
          (should (equal (get-text-property (string-match "<<" onto-text) 'face onto-text)
                         '(:inherit shadow :weight bold)))
          (should (string-match-p (regexp-quote (majjic--short-id target-change-id)) onto-text))
          (should-not (string-match-p (regexp-quote (majjic--short-id target-commit-id)) onto-text)))
        (majjic-abandon-start)
        (should (eq (majjic-rebase-state-target-mode majjic--rebase-state) 'after))
        (majjic-rebase-set-source-descendants)
        (should (eq (majjic-rebase-state-source-mode majjic--rebase-state) 'descendants))
        (should (seq-some (lambda (overlay)
                            (let ((text (or (overlay-get overlay 'before-string)
                                            (overlay-get overlay 'after-string))))
                              (and (stringp text)
                                   (string-match-p "itself and descendants of" text))))
                          majjic--rebase-overlays))
        (majjic-rebase-start)
        (should (eq (majjic-rebase-state-source-mode majjic--rebase-state) 'revision))
        (should (= (seq-count (lambda (overlay)
                                (let ((text (overlay-get overlay 'before-string)))
                                  (and (stringp text)
                                       (string-match-p "<< move >>" text))))
                              majjic--rebase-overlays)
                 1))
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
            (majjic--goto-commit source)
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
        (let* ((_child (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id"))
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
              (majjic-test--wait-for-idle)
              (should (equal (majjic--change-id-for-commit-id (majjic--current-commit-id))
                             child-change))
              (should-not majjic-rebase-mode)
              (should (equal (jj "log" "-r" (format "parents(%s)" (majjic--current-commit-id))
                                 "--no-graph" "--color" "never" "--template" "commit_id")
                             side)))))))))

(ert-deftest majjic-test-squash-marked-sources-into-destination ()
  "Applying squash should move marked source changes into the selected destination."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-squash-" t))
         (default-directory repo))
    (cl-labels ((jj (&rest args)
                    (let ((default-directory repo))
                      (with-temp-buffer
                        (let ((exit (apply #'process-file "jj" nil t nil args)))
                          (unless (zerop exit)
                            (error "jj %S failed: %s" args (buffer-string)))
                          (string-trim (buffer-string)))))))
      (jj "git" "init" ".")
      (let ((root (jj "log" "-r" "@"
                      "--no-graph" "--color" "never" "--template" "commit_id")))
        (jj "new" root)
        (majjic-test--write-repo-file repo "a.txt" "a\n")
        (jj "describe" "-m" "source one")
        (let ((source-one (jj "log" "-r" "@"
                              "--no-graph" "--color" "never" "--template" "commit_id"))
              (source-one-change (jj "log" "-r" "@"
                                     "--no-graph" "--color" "never" "--template" "change_id")))
          (jj "new" root)
          (majjic-test--write-repo-file repo "b.txt" "b\n")
          (jj "describe" "-m" "source two")
          (let ((source-two (jj "log" "-r" "@"
                                "--no-graph" "--color" "never" "--template" "commit_id"))
                (source-two-change (jj "log" "-r" "@"
                                       "--no-graph" "--color" "never" "--template" "change_id")))
            (jj "new" root)
            (majjic-test--write-repo-file repo "c.txt" "c\n")
            (jj "describe" "-m" "destination")
            (let ((destination-change (jj "log" "-r" "@"
                                          "--no-graph" "--color" "never" "--template" "change_id")))
              (majjic)
              (with-current-buffer (majjic--log-buffer-name repo)
                (majjic--goto-commit source-one)
                (majjic-toggle-mark)
                (majjic--goto-commit source-two)
                (majjic-toggle-mark)
                (majjic-squash-start)
                (should (equal (sort (copy-sequence
                                      (majjic-squash-state-source-change-ids
                                       majjic--squash-state))
                                     #'string<)
                               (sort (list source-one-change source-two-change)
                                     #'string<)))
                (should (equal (majjic--current-commit-id) root))
                (majjic--goto-commit (majjic--commit-id-for-change-id destination-change))
                (majjic-squash-apply)
                (majjic-test--wait-for-idle)
                (should-not majjic-squash-mode)
                (should-not (majjic--commit-id-for-change-id source-one-change))
                (should-not (majjic--commit-id-for-change-id source-two-change))
                (let* ((destination (majjic--commit-id-for-change-id destination-change))
                       (summary (jj "diff" "--summary" "-r" destination)))
                  (should (equal (majjic--current-commit-id) destination))
                  (should (string-match-p "A a\\.txt" summary))
                  (should (string-match-p "A b\\.txt" summary))
                  (should (string-match-p "A c\\.txt" summary)))))))))))

(ert-deftest majjic-test-rebase-descendants-apply-and-reject-internal-target ()
  "Descendants source mode should move the subtree and reject targets inside it."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-rebase-descendants-" t))
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
        (let ((child (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id"))
              (child-change (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "change_id")))
          (jj "new")
          (with-temp-file (expand-file-name "f.txt" repo)
            (insert "grandchild\n"))
          (jj "describe" "-m" "grandchild")
          (let ((grandchild-change (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "change_id")))
            (jj "new" root)
            (with-temp-file (expand-file-name "g.txt" repo)
              (insert "side\n"))
            (jj "describe" "-m" "side")
            (let ((side (jj "log" "-r" "@" "--no-graph" "--color" "never" "--template" "commit_id")))
              (majjic)
              (with-current-buffer (majjic--log-buffer-name repo)
                (majjic--goto-commit child)
                (majjic-rebase-start)
                (majjic-rebase-set-source-descendants)
                (goto-char (point-min))
                (should (search-forward "grandchild" nil t))
                (beginning-of-line)
                (should-error (majjic-rebase-apply) :type 'user-error)
                (should majjic-rebase-mode)
                (should majjic--rebase-overlays)
                (goto-char (point-min))
                (should (search-forward "side" nil t))
                (beginning-of-line)
                (majjic-rebase-apply)
                (majjic-test--wait-for-idle)
                (should-not majjic-rebase-mode)
                (let* ((rebased-child (majjic--commit-id-for-change-id child-change))
                       (rebased-grandchild (majjic--commit-id-for-change-id grandchild-change)))
                  (should (equal (jj "log" "-r" (format "parents(%s)" rebased-child)
                                     "--no-graph" "--color" "never" "--template" "commit_id")
                                 side))
                  (should (equal (jj "log" "-r" (format "parents(%s)" rebased-grandchild)
                                     "--no-graph" "--color" "never" "--template" "commit_id")
                                 rebased-child)))))))))))

(ert-deftest majjic-test-undo-redo-roundtrip ()
  "Undo and redo should roundtrip a simple repository mutation through the UI."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-undo-" t))
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
        (insert "one\n"))
      (jj "describe" "-m" "one")
      (let ((first (jj "log" "-r" "@" "--no-graph" "--color" "never"
                       "--template" "commit_id")))
        (majjic)
        (with-current-buffer (majjic--log-buffer-name repo)
          (majjic-new)
          (majjic-test--wait-for-idle)
          (let ((second (jj "log" "-r" "@" "--no-graph" "--color" "never"
                            "--template" "commit_id")))
            (should-not (equal first second))
            (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
                       (lambda (_action) t)))
              (majjic-undo))
            (majjic-test--wait-for-idle)
            (should (equal (jj "log" "-r" "@" "--no-graph" "--color" "never"
                               "--template" "commit_id")
                           first))
            (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
                       (lambda (_action) t)))
              (majjic-redo))
            (majjic-test--wait-for-idle)
            (should (equal (jj "log" "-r" "@" "--no-graph" "--color" "never"
                            "--template" "commit_id")
                           second))))))))

(ert-deftest majjic-test-undo-redo-reselects-rewritten-change ()
  "Undo and redo should keep the same logical change selected across rewrites."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-undo-rewrite-" t))
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
        (insert "one\n"))
      (jj "describe" "-m" "one")
      (let* ((original-commit (jj "log" "-r" "@" "--no-graph" "--color" "never"
                                  "--template" "commit_id"))
             (change-id (jj "log" "-r" "@" "--no-graph" "--color" "never"
                            "--template" "change_id")))
        (jj "describe" "-m" "one updated")
        (let ((rewritten-commit (jj "log" "-r" "@" "--no-graph" "--color" "never"
                                    "--template" "commit_id")))
          (should-not (equal original-commit rewritten-commit))
          (majjic)
          (with-current-buffer (majjic--log-buffer-name repo)
            (goto-char (point-min))
            (should (search-forward "one updated" nil t))
            (beginning-of-line)
            (should (equal (majjic--current-commit-id) rewritten-commit))
            (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
                       (lambda (_action) t)))
              (majjic-undo))
            (majjic-test--wait-for-idle)
            (should (equal (majjic--change-id-for-commit-id (majjic--current-commit-id))
                           change-id))
            (should (equal (majjic--current-commit-id) original-commit))
            (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
                       (lambda (_action) t)))
              (majjic-redo))
            (majjic-test--wait-for-idle)
            (should (equal (majjic--change-id-for-commit-id (majjic--current-commit-id))
                           change-id))
            (should (equal (majjic--current-commit-id) rewritten-commit))))))))

(ert-deftest majjic-test-redo-with-nothing-to-redo-leaves-normal-state ()
  "A failed redo should surface via the mutation path and leave no transient mode active."
  (skip-unless (executable-find "jj"))
  (let* ((repo (make-temp-file "majjic-redo-empty-" t))
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
        (insert "one\n"))
      (jj "describe" "-m" "one")
      (majjic)
      (with-current-buffer (majjic--log-buffer-name repo)
        (cl-letf (((symbol-function 'majjic--confirm-latest-operation)
                   (lambda (_action) t)))
          (majjic-redo))
        (majjic-test--wait-for-idle)
        (should-not majjic-rebase-mode)
        (should (derived-mode-p 'majjic-log-mode))))))

(provide 'majjic-tests)

;;; majjic-tests.el ends here
