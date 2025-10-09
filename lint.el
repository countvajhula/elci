;; lint.el
;; This script runs package-lint on the packages.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)

;; Install the linter. straight.el will find the
;; recipe for this package in the local `xelpa` repository.
;; If there's no recipe there, it will look on standard configured
;; archives. If the client repo is using the package suite pattern
;; (i.e., multiple packages sharing a common project namespace), the
;; github.com/countvajhula/package-lint fork should be used.
(straight-use-package 'package-lint)


;; --- The Linter Tool ---
(defun ci-lint-package (pkg-name)
  "Run package-lint on PKG-NAME, print all output,
and return a shell-friendly exit code."
  (let* ((linter-dir (straight--build-dir "package-lint"))
         (load-path-args (ci-get-load-path-args pkg-name (list linter-dir)))
         (main-file (ci-get-package-main-file pkg-name))
         ;; Use the helper to get a clean list of source files to lint.
         (files-to-lint (ci-get-package-source-files pkg-name))
         (output-buffer (generate-new-buffer " *lint-output*"))
         ;; The lint prefix is either the project name (for suites) or the
         ;; single package name.
         (lint-prefix (or ci-project-name (car ci-packages))))

    (message (format "--- Linting %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             ;; --- Configure package.el for the linter ---
                             ;; Although Emacs CI uses Straight.el to find and build
                             ;; package sources, package-lint uses package.el
                             ;; internally to check the "installability" of package dependencies.
                             ;; In order for this to produce an accurate result, we
                             ;; configure package.el and, in particular, add MELPA
                             ;; to its list of known archives.
                             ;; This ensures package-lint can find dependencies on MELPA.
                             ;; Note that this config is effectively ignored if the client
                             ;; repo is using countvajhula/package-lint, as that does not
                             ;; do any installability checks (as those are covered by other,
                             ;; dedicated, CI facilities).
                             '("--eval"
                               "(progn \
                                  (require 'package) \
                                  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
                                  (package-initialize)
                                  (package-refresh-contents))")
                             ;; ----------------------------------------------------
                             ;; Set all necessary linter variables.
                             (list "--eval"
                                   (format "(setq package-lint-prefix %S
                                                  package-lint-main-file %S
                                                  package-lint-check-installable nil)"
                                           lint-prefix main-file))
                             '("-l" "package-lint")
                             '("-f" "package-lint-batch-and-exit")
                             files-to-lint))
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil output-buffer nil args)))
          (with-current-buffer output-buffer
            (princ (buffer-string)))
          exit-code)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
(let ((exit-code 0))
  (dolist (pkg ci-packages)
    (let ((status (ci-lint-package pkg)))
      (unless (zerop status)
        (message (format "\n!!! Linting failed for %s with status %d" pkg status))
        (setq exit-code status))))
  (if (zerop exit-code)
      (message "\nAll packages passed linting.")
    (kill-emacs exit-code)))
