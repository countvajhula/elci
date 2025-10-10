;; test.el
;; This script runs the project's ERT tests.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory to the load-path so we can `require`
;; modules like ci.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)

;; Install the test runner dependency.
(straight-use-package 'ert-runner)


;; --- The Test Runner Tool ---
(defun ci-test-package (pkg-name)
  "Run ERT tests for PKG-NAME, print all output,
and return a shell-friendly exit code."
  (let* ((repo-root (expand-file-name ".."))
         (is-suite ci-project-name)
         (relative-dir (if is-suite pkg-name "."))
         (source-dir (expand-file-name relative-dir repo-root))
         (test-dir (expand-file-name "test" source-dir))
         ;; Add all installed packages to the load-path for a robust environment.
         (build-root (expand-file-name "straight/build" straight-base-dir))
         (all-build-dirs (directory-files build-root t))
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 all-build-dirs))
         (output-buffer (generate-new-buffer " *test-output*")))

    (message (format "\n--- Running tests for %s ---" pkg-name))

    (if (not (file-directory-p test-dir))
        (progn
          (message "No 'test/' directory found for this package. Skipping.")
          0) ; Return success.

      (unwind-protect
          ;; This command changes the CWD to the project root and then tells
          ;; ert-runner to run tests found in the package's specific test directory.
          (let* ((args (append '("-Q" "--batch")
                               load-path-args
                               (list "--eval" (format "(cd %S)" repo-root))
                               '("-l" "ert-runner")
                               ;; Pass the specific test directory as an argument.
                               (list test-dir)))
                 (exit-code (apply #'call-process
                                   (executable-find "emacs") nil output-buffer nil args)))
            (with-current-buffer output-buffer
              (princ (buffer-string)))
            exit-code)
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))


;; --- Main Execution ---
(let ((overall-exit-code 0))
  (dolist (pkg ci-packages)
    (let ((status (ci-test-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Tests failed for %s." pkg))
        ;; Keep track of the first non-zero exit code.
        (setq overall-exit-code (or overall-exit-code status)))))

  (if (zerop overall-exit-code)
      (message "\nAll tests passed.")
    (progn
      (message "\n!!! Some tests failed.")
      (kill-emacs overall-exit-code))))

