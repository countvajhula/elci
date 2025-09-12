;; test.el
;; This script runs the package's ERT tests.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path so we can `require`
;; modules like ci.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)
(ci-load-optional-deps)

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
         (files-to-test (when (file-directory-p test-dir)
                          (directory-files-recursively test-dir "\\-test\\.el$")))
         ;; --- MODIFIED LOGIC ---
         ;; Instead of trying to resolve dependencies, which is fragile
         ;; across CI steps, we will simply add every directory inside the
         ;; `straight/build` directory to the load-path. This is robust
         ;; because the `install` step is the single source of truth for
         ;; what packages are available.
         (build-root (expand-file-name "straight/build" straight-base-dir))
         (all-build-dirs (directory-files build-root t))
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 all-build-dirs))
         ;; --- END MODIFIED LOGIC ---
         (output-buffer (generate-new-buffer " *test-output*")))

    (message (format "--- Testing %s ---" pkg-name))

    (if (not files-to-test)
        (progn (message "No tests found.") 0) ; Return success if no tests exist.

      (unwind-protect
          (let* ((args (append '("-Q" "--batch")
                               load-path-args
                               '("-l" "ert-runner")
                               '("-f" "ert-runner-run-tests-batch-and-exit")
                               files-to-test))
                 (exit-code (apply #'call-process
                                   (executable-find "emacs") nil output-buffer nil args)))
            (with-current-buffer output-buffer
              (princ (buffer-string)))
            exit-code)
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))


;; --- Main Execution ---
(let ((exit-code 0))
  (dolist (pkg ci-packages)
    (let ((status (ci-test-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Tests failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll tests passed.")
    (kill-emacs exit-code)))
