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
(ci-load-optional-deps)

(setq straight-allow-recipe-inheritance nil)


;; --- Install project packages ---
(message "--- Installing project packages ---")

(dolist (pkg ci-packages)
  ;; Call with `t` to enable `:fork t` for the initial install,
  ;; which overrides any remote recipe archives that may host
  ;; these packages.
  (ci-install-package pkg t))

(message "--- Package installation complete ---")
