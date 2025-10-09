;; lint.el
;; This script runs package-lint on the packages.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)

;; Install package-lint. straight.el will find the recipe for this in the
;; project's local `xelpa` repository if it exists, or on MELPA otherwise.
(straight-use-package 'package-lint)


;; --- The Linter Tool ---
(defun ci-lint-package (pkg-name)
  "Run package-lint on PKG-NAME, print all output,
and return a shell-friendly exit code."
  (let* ((linter-dir (straight--build-dir "package-lint"))
         (load-path-args (ci-get-load-path-args pkg-name (list linter-dir)))
         (main-file (ci-get-package-main-file pkg-name))
         (files-to-lint (ci-get-package-source-files pkg-name))
         (output-buffer (generate-new-buffer " *lint-output*"))
         (lint-prefix (or ci-project-name (car ci-packages)))
         (xelpa-path (expand-file-name "xelpa")))

    (message (format "--- Linting %s ---" pkg-name))
    (unwind-protect
        (let* ((debug-and-setup-program
                `(progn
                   (require 'package) ; Load the package library first.

                   (message "--- Linter Subprocess Debug ---")
                   (add-to-list 'package-archives '("xelpa" . ,xelpa-path) t)
                   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
                   (message "  - package-archives set to: %S" package-archives)

                   (message "  - Running (package-initialize) and (package-refresh-contents)...")
                   (condition-case err
                       (progn
                         (package-initialize)
                         ;; This is the crucial step: actually fetch the package lists.
                         (package-refresh-contents))
                     (error (message "  - ERROR during package setup: %S" err)))
                   (message "  - Package setup complete.")
                   (message "  - package-archive-contents is %s"
                            (if package-archive-contents "populated" "NIL"))

                   (setq package-lint-prefix ,lint-prefix)
                   (setq package-lint-main-file ,main-file)
                   (message "---------------------------------")))
               (args (append '("-Q" "--batch")
                             load-path-args
                             ;; Run the debug and setup program.
                             (list "--eval" (format "%S" debug-and-setup-program))
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

