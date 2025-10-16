;;; agent-shell-completion-tests.el --- Tests for agent-shell-completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-completion)

;;; Code:

(ert-deftest agent-shell-completion--extract-all-directory-components-test ()
  "Test directory component extraction."
  ;; Test nested path
  (should (equal (agent-shell-completion--extract-all-directory-components "a/b/c/file.txt")
                 '("a" "a/b" "a/b/c")))

  ;; Test single directory
  (should (equal (agent-shell-completion--extract-all-directory-components "dir/file.txt")
                 '("dir")))

  ;; Test file with no directory
  (should (equal (agent-shell-completion--extract-all-directory-components "file.txt")
                 nil))

  ;; Test trailing slash
  (should (equal (agent-shell-completion--extract-all-directory-components "a/b/c/")
                 '("a" "a/b" "a/b/c")))

  ;; Test empty string
  (should (equal (agent-shell-completion--extract-all-directory-components "")
                 nil)))

(ert-deftest agent-shell-completion--make-file-completion-table-test ()
  "Test completion table creation."
  (let ((candidates '("file1.txt" "file2.txt" "dir/file3.txt"))
        (table (agent-shell-completion--make-file-completion-table
                '("file1.txt" "file2.txt" "dir/file3.txt"))))

    ;; Test that it returns metadata
    (should (equal (funcall table "" nil 'metadata)
                   '(metadata (category . file))))

    ;; Test that it completes candidates
    (let ((completions (funcall table "file" nil t)))
      (should (member "file1.txt" completions))
      (should (member "file2.txt" completions)))

    ;; Test all completions
    (let ((all (funcall table "" nil t)))
      (should (equal (length all) 3)))))

(ert-deftest agent-shell-completion--directory-completion-test ()
  "Test directory-based completion."
  ;; Create temp directory with test files
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (file1 (expand-file-name "test1.txt" temp-dir))
         (file2 (expand-file-name "test2.el" temp-dir))
         (subdir (expand-file-name "subdir" temp-dir)))

    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file file1 (insert "test1"))
          (with-temp-file file2 (insert "test2"))
          (make-directory subdir)

          ;; Test completion with no prefix
          (let ((result (agent-shell-completion--directory-completion
                         0 0 temp-dir "" #'identity temp-dir)))
            (should result)
            ;; Result is (start end table :annotation-function func)
            (should (>= (length result) 4))
            (should (= (nth 0 result) 0))
            (should (= (nth 1 result) 0)))

          ;; Test completion with prefix
          (let ((result (agent-shell-completion--directory-completion
                         0 0 temp-dir "test1" #'identity temp-dir)))
            (should result)))

      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest agent-shell-completion--file-mention-completion-at-point-test ()
  "Test file mention completion at point."
  ;; Mock agent-shell-cwd to return temp directory
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (file1 (expand-file-name "test.txt" temp-dir)))

    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file file1 (insert "test"))

          ;; Mock agent-shell-cwd
          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () temp-dir))
                    ((symbol-function 'project-current)
                     (lambda (&optional _) nil)))  ; No project

            (with-temp-buffer
              ;; Test completion after @ symbol
              (insert "test @te")
              (let ((result (agent-shell-completion--file-mention-completion-at-point)))
                (should result)
                ;; Buffer positions are 1-indexed: "test @te"
                ;; Position 7 is the 't' after @, position 9 is after 'e'
                (should (= (nth 0 result) 7))   ; start position after @
                (should (= (nth 1 result) 9)))  ; end position

              ;; Test no completion when not after @
              (erase-buffer)
              (insert "test file")
              (should-not (agent-shell-completion--file-mention-completion-at-point))

              ;; Test completion with @
              (erase-buffer)
              (insert "@")
              (let ((result (agent-shell-completion--file-mention-completion-at-point)))
                (should result)))))

      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest agent-shell-completion--fuzzy-matching-test ()
  "Test that fuzzy matching works (no pre-filtering)."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (file1 (expand-file-name "test-file.txt" temp-dir))
         (file2 (expand-file-name "my-test.el" temp-dir))
         (file3 (expand-file-name "other.txt" temp-dir)))

    (unwind-protect
        (progn
          (with-temp-file file1 (insert "test1"))
          (with-temp-file file2 (insert "test2"))
          (with-temp-file file3 (insert "test3"))

          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () temp-dir))
                    ((symbol-function 'project-current)
                     (lambda (&optional _) nil)))

            (with-temp-buffer
              ;; Type @te - should get ALL files, not just ones starting with "te"
              (insert "@te")
              (let* ((result (agent-shell-completion--file-mention-completion-at-point))
                     (table (nth 2 result))
                     (all-candidates (funcall table "" nil t)))
                ;; Should include files that DON'T start with "te"
                (should (member "my-test.el" all-candidates))
                (should (member "test-file.txt" all-candidates))
                (should (member "other.txt" all-candidates))))))

      (delete-directory temp-dir t))))

(ert-deftest agent-shell-completion--quoted-filename-test ()
  "Test completion with quoted filenames."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (file1 (expand-file-name "file with spaces.txt" temp-dir)))

    (unwind-protect
        (progn
          (with-temp-file file1 (insert "test"))

          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () temp-dir))
                    ((symbol-function 'project-current)
                     (lambda (&optional _) nil)))

            (with-temp-buffer
              ;; Test completion after @"
              (insert "test @\"file")
              (let ((result (agent-shell-completion--file-mention-completion-at-point)))
                (should result)
                ;; Position after @" is 8, "file" ends at 12
                (should (= (nth 0 result) 8))   ; start after @"
                (should (= (nth 1 result) 12))) ; end after "file"

              ;; Test completion with just @"
              (erase-buffer)
              (insert "@\"")
              (let ((result (agent-shell-completion--file-mention-completion-at-point)))
                (should result)))))

      (delete-directory temp-dir t))))

(provide 'agent-shell-completion-tests)
;;; agent-shell-completion-tests.el ends here
