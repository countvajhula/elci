;; install.el
;; This script installs all packages defined in the project's recipe files.
;; It must be run *after* bootstrap.el.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path so we can `require`
;; modules like ci.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'ci)
(ci-load-straight)

(setq straight-allow-recipe-inheritance nil)

;; --- Install project packages ---
(message "--- Installing project packages ---")

;; The list of packages comes from the CI_PACKAGES env var.
;; straight.el will find the recipes for these packages in the
;; generated `xelpa` recipe repository.
(dolist (pkg ci-packages)
  (straight-use-package (intern pkg)))

(message "--- Package installation complete ---")
