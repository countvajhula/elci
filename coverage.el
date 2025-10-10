;; coverage.el
;; This script runs the package's tests with coverage reporting.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory to the load-path.
(add-to-list 'load-path ".")

;; Load shared helpers and dependencies.
(require 'ci)
(ci-load-straight)
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
         (build-root (expand-file-name "straight/build" straight-base-dir))
         (all-build-dirs (directory-files build-root t))
         ;; The load-path MUST include the repo-root for instrumentation.
         (load-path-dirs (append (list repo-root) all-build-dirs))
         (load-path-args (mapcan (lambda (dir)
                                   (when (file-directory-p dir)
                                     (list "-L" (directory-file-name dir))))
                                 load-path-dirs))
         (output-buffer (generate-new-buffer " *coverage-output*")))

    (message (format "\n--- Running coverage for %s ---" pkg-name))

    (if (not (file-directory-p test-dir))
        (progn
          (message "No 'test/' directory found for this package. Skipping.")
          0) ; Return success.

      (unwind-protect
          ;; This command uses the canonical, hook-based invocation pattern.
          (let* ((args (append '("-Q" "--batch")
                               load-path-args
                               ;; Set the working directory for the subprocess.
                               (list "--eval" (format "(cd %S)" repo-root))
                               '("--eval" "(setq load-prefer-newer nil)")
                               '("-l" "undercover")
                               '("--eval" "(undercover)")
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
    (let ((status (ci-coverage-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Coverage run failed for %s." pkg))
        ;; Keep track of the first non-zero exit code.
        (setq overall-exit-code (or overall-exit-code status)))))

  (if (zerop overall-exit-code)
      (message "\nCoverage run completed successfully.")
    (progn
      (message "\n!!! Some coverage runs failed.")
      (kill-emacs overall-exit-code))))

