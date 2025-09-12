;; coverage.el
;; This script runs the package's ERT tests with coverage reporting.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path so we can `require`
;; other generic scripts like ci.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)
(ci-load-optional-deps)

;; Install the test runner and coverage dependencies.
(straight-use-package 'ert-runner)
(straight-use-package 'undercover)


;; --- The Coverage Runner Tool ---
(defun ci-coverage-package (pkg-name)
  "Run ERT tests for PKG-NAME with coverage, print all output,
and return a shell-friendly exit code."
  (let* ((repo-root (expand-file-name ".."))
         (is-suite ci-project-name)
         (relative-dir (if is-suite pkg-name "."))
         (source-dir (expand-file-name relative-dir repo-root))
         (test-dir (expand-file-name "test" source-dir))
         (files-to-test (when (file-directory-p test-dir)
                          (directory-files-recursively test-dir "\\-test\\.el$")))
         ;; Add all installed packages to the load-path for a robust environment.
         (build-root (expand-file-name "straight/build" straight-base-dir))
         (all-build-dirs (directory-files build-root t))
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 all-build-dirs))
         (output-buffer (generate-new-buffer " *coverage-output*")))

    (message (format "--- Running coverage for %s ---" pkg-name))

    (if (not files-to-test)
        (progn (message "No tests found.") 0) ; Return success if no tests.

      (unwind-protect
          ;; The command first loads `undercover`, then calls the `(undercover)`
          ;; function to install its hooks, and finally loads `ert-runner`
          ;; to run the tests. `undercover` automatically detects CI
          ;; environment variables (like COVERALLS_REPO_TOKEN).
          (let* ((args (append '("-Q" "--batch")
                               load-path-args
                               '("-l" "undercover")
                               '("--eval" "(undercover)")
                               '("-l" "ert-runner")
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
    (let ((status (ci-coverage-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Coverage run failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nCoverage run completed successfully.")
    (kill-emacs exit-code)))
