;; native-compile.el
;; This script native-compiles the packages.
;; It must be run *after* install.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Load the shared CI helper functions and constants.
(require 'ci (expand-file-name "ci.el"))
(ci-load-straight)
(ci-load-optional-deps)

;; --- The Direct Native Compilation Tool ---
(defun ci-native-compile-package (pkg-name)
  "Native-compile PKG-NAME using `batch-native-compile`, print all output,
and return a shell-friendly exit code."
  ;; First, ensure the package and its dependencies are known to this session.
  (ci-install-package pkg-name)

  (let* ((load-path-args (ci-get-load-path-args pkg-name))
         ;; For compilation, we want all .el files, including autoloads.
         (files-to-compile (ci-get-package-all-files pkg-name))
         (output-buffer (generate-new-buffer " *compilation-output*")))

    (message (format "--- Native-Compiling %s ---" pkg-name))
    (unwind-protect
        (let* ((args (append '("-Q" "--batch")
                             load-path-args
                             ;; Emacs reuses the byte-compile error flag for native comp too.
                             '("--eval" "(setq byte-compile-error-on-warn t)")
                             '("-f" "batch-native-compile")
                             files-to-compile))
               (exit-code (apply #'call-process
                                 (executable-find "emacs") nil output-buffer nil args)))
          (with-current-buffer output-buffer
            (princ (buffer-string)))
          exit-code)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


;; --- Main Execution ---
;; Only run these checks if native compilation is actually available.
(if (not (featurep 'native-compile))
    (message "Native compilation not supported by this Emacs version. Skipping.")
  (let ((exit-code 0))
    (dolist (pkg ci-packages)
      (let ((status (ci-native-compile-package pkg)))
        (unless (zerop status)
          (message (format "\n!!! Native compilation failed for %s with status %d" pkg status))
          (setq exit-code status))))
    (if (zerop exit-code)
        (message "\nAll packages native-compiled successfully.")
      (kill-emacs exit-code))))
