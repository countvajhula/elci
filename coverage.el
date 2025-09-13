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
         ;; The load-path MUST include the repo-root itself so that the
         ;; instrumented source files can be found and required by the tests.
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 (append (list repo-root) all-build-dirs)))
         (output-buffer (generate-new-buffer " *coverage-output*")))

    (message (format "--- Running coverage for %s ---" pkg-name))

    (if (not files-to-test)
        (progn (message "No tests found.") 0)

      (unwind-protect
          (let* ((undercover-config-env (getenv "UNDERCOVER_CONFIG"))
                 (program
                  `(progn
                     ;; Set the working directory inside the subprocess to the
                     ;; project root. This ensures that relative paths in the
                     ;; undercover config are resolved correctly.
                     (let ((default-directory ,repo-root))
                       ;; Tell Emacs to prefer source .el files over compiled
                       ;; .elc files. This is essential for undercover's
                       ;; instrumentation to work.
                       (setq load-prefer-newer nil)
                       (require 'ert-runner)
                       (require 'undercover)
                       ;; If UNDERCOVER_CONFIG is set, read and use it.
                       ;; Otherwise, use undercover's default behavior (for Coveralls).
                       ,(if undercover-config-env
                            `(undercover (read ,undercover-config-env))
                          `(undercover))
                       ;; Now, run the tests on the instrumented code.
                       (apply #'ert-runner-run-tests-batch ',files-to-test))))
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

