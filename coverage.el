;; coverage.el
;; This script runs the package's tests with coverage reporting.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path.
(add-to-list 'load-path ".")

;; Load shared helpers and dependencies.
(require 'ci)
(ci-load-straight)
(ci-load-optional-deps)
(straight-use-package 'ert-runner)
(straight-use-package 'undercover)

;; --- The Coverage Runner Tool ---
(defun ci-coverage-package (pkg-name)
  "Run ERT tests with coverage for PKG-NAME, print all output,
and return a shell-friendly exit code."
  (let* ((repo-root (expand-file-name ".."))
         (is-suite ci-project-name)
         (relative-dir (if is-suite pkg-name "."))
         (source-dir (expand-file-name relative-dir repo-root))
         (test-dir (expand-file-name "test" source-dir))
         (files-to-test (when (file-directory-p test-dir)
                          (directory-files-recursively test-dir "\\-test\\.el$")))
         (build-root (expand-file-name "straight/build" straight-base-dir))
         (all-build-dirs (directory-files build-root t))
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 (append (list repo-root) all-build-dirs)))
         (output-buffer (generate-new-buffer " *coverage-output*")))

    (message (format "--- Running coverage for %s ---" pkg-name))

    (let ((pkg-build-dir (straight--build-dir pkg-name)))
      (message (format "Cleaning compiled files in %s..." pkg-build-dir))
      (dolist (file (directory-files-recursively pkg-build-dir "\\.elc$"))
        (delete-file file)))

    (if (not files-to-test)
        (progn (message "No tests found.") 0)

      (unwind-protect
          (let* ((program
                  `(progn
                     ;; Set the working directory for the entire subprocess.
                     ;; This is crucial for undercover's report generation.
                     (cd ,repo-root)
                     (require 'ert-runner)
                     (require 'undercover)
                     ;; undercover will automatically read its configuration from
                     ;; the UNDERCOVER_CONFIG environment variable.
                     (undercover)
                     ;; Run the tests on the instrumented code.
                     (apply #'ert-runner-run-tests-batch ',files-to-test)
                     ;; Manually generate the report after the tests are done.
                     ;; This is more robust than relying on shutdown hooks.
                     (undercover-report)))
                 (args (append '("-Q" "--batch")
                               load-path-args
                               (list "--eval" (format "%S" program))))
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

