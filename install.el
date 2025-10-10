;; install.el
;; This script installs all packages defined in the project's recipe files
;; and shows the contents of their build directories.
;; It must be run *after* bootstrap.el has successfully completed.
;; -*- lexical-binding: t -*-

;; Add the current directory to the load-path.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)

(setq straight-allow-recipe-inheritance nil)


;; --- Install project packages ---
(message "--- Installing project packages ---")

(dolist (pkg ci-packages)
  (message (format "\n--- Installing %s ---" pkg))
  ;; Install the package using its recipe from `xelpa`.
  (straight-use-package (intern pkg))

  ;; --- Show contents of build directory ---
  ;; This provides visibility into the result of the `:files` directive
  ;; in the package's recipe, allowing the user to verify its correctness.
  (let* ((build-dir (straight--build-dir pkg))
         (output-buffer (generate-new-buffer " *build-contents*")))
    (message (format "\n--- Contents of build directory for %s ---" pkg))
    (message (format "%s" build-dir))

    (if (not (file-directory-p build-dir))
        (message "Build directory not found.")
      (unwind-protect
          ;; Use `find` to get a clean, recursive listing.
          ;; We run it from within the build directory to get relative paths.
          (let* ((default-directory build-dir)
                 (exit-code (call-process "find" nil output-buffer nil "." "-print")))
            (with-current-buffer output-buffer
              (princ (buffer-string)))
            exit-code)
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))

(message "\n--- Package installation complete ---")
