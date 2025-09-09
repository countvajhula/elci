;; install.el
;; This script is the main entry point for installing all packages.
;; It loads external dependency definitions and then installs the project's
;; own packages. It must be run *after* bootstrap.el.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path so we can `require`
;; other generic scripts like helpers.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'helpers)
(ci-load-straight)

;; Optionally load project-specific external dependencies from the project's
;; own `ci/` directory, which is one level up from this script's location.
(let ((project-ci-dir (expand-file-name "../ci")))
  (when (file-exists-p (expand-file-name "ci-deps.el" project-ci-dir))
    (add-to-list 'load-path project-ci-dir)
    (require 'ci-deps)))

(setq straight-allow-recipe-inheritance nil)


;; --- Install project packages ---
(message "--- Installing project packages ---")

(dolist (pkg ci-packages)
  (ci-install-package pkg))

(message "--- Package installation complete ---")
