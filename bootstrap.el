;; bootstrap.el
;; This script bootstraps straight.el and generates the local `xelpa`
;; recipe repository from the project's `.ci/recipes.eld` file.
;; -*- lexical-binding: t -*-

(require 'base (expand-file-name "base.el"))

(defvar straight-base-dir (expand-file-name "init"))

(message "--- Bootstrapping straight.el ---")

;; --- Bootstrap straight.el ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        straight-base-dir))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-base-dir (expand-file-name "init"))

(setq straight-allow-recipe-inheritance nil)

(message "--- Bootstrap complete ---")

;; --- Install Elacarte and build local recipe repository ---
(let* ((repo-root (expand-file-name ".."))
       (elci-recipes (expand-file-name "ci-recipes.eld"))
       (project-recipes (expand-file-name "recipes.eld" repo-root))
       (project-ci-recipes (expand-file-name ".ci/recipes.eld" repo-root)))
  (message "--- Installing Elacarte and building local recipe repository ---")
  ;; 1. install elacarte and set the elacarte-base-dir
  ;; Although the recipe is present in elci's recipes.eld, Elacarte itself
  ;; is needed before we could build and use the local recipe repository
  ;; that would house this recipe and allow Straight to install it!
  ;; In order to minimally "bootstrap" it, we simply read this recipe
  ;; directly from recipes.eld, and install it using that recipe inline
  ;; in this call for Straight to install it.
  ;; Future use of Elacarte needn't do this as long as the built local
  ;; recipe repository is known to Straight.
  (bootstrap-elacarte elci-recipes)
  (require 'elacarte)
  ;; 2. add recipes in the order: elci, project, project's .ci. [don't support "."]
  (if (file-exists-p elci-recipes)
      (elacarte-add-recipes-in-file elci-recipes)
    (warn "No recipes file found in Elci for CI."))
  (if (file-exists-p project-recipes)
      (elacarte-add-recipes-in-file project-recipes)
    (warn "No recipes file found in the project."))
  (if (file-exists-p project-ci-recipes)
      (elacarte-add-recipes-in-file project-ci-recipes)
    (warn "No recipes file found in the project's CI overrides."))
  ;; 3. set up local elacarte recipe repo
  (elacarte-build-recipe-repository)
  (elacarte-register-recipe-repository)
  (message "--- Elacarte done ---"))
