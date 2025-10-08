;; bootstrap.el
;; This script bootstraps straight.el and generates the local `xelpa`
;; recipe repository from the project's `.ci/recipes.el` file.
;; -*- lexical-binding: t -*-

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

(message "--- Bootstrap complete ---")


;; --- Generate local 'xelpa' recipe repository ---
(let* ((repo-root (expand-file-name ".."))
       (project-recipe-file (expand-file-name ".ci/recipes.el" repo-root))
       (xelpa-dir (expand-file-name "xelpa"))
       (xelpa-recipes-dir (expand-file-name "recipes" xelpa-dir)))
  (when (file-exists-p project-recipe-file)
    (message "--- Generating local 'xelpa' recipe repository ---")
    ;; Clean and create the target directory.
    (when (file-directory-p xelpa-recipes-dir)
      (delete-directory xelpa-recipes-dir t))
    (make-directory xelpa-recipes-dir t)

    ;; Read the list of recipes from the project's file.
    ;; The recipes could either be verbatim lists that would
    ;; be used inline as a straight-use-package recipe, e.g.,
    ;; (my-package :type git :host github :files ("*.el") ...)
    ;; or they could be alist-formatted, like so:
    ;; (my-package . (:type git :host github :files ("*.el") ...))
    (let ((recipes (with-temp-buffer
                     (insert-file-contents project-recipe-file)
                     (read (current-buffer)))))
      (dolist (recipe recipes)
        (let* ((recipe-id (car recipe))
               ;; Handle recipe ID being either a symbol or a string.
               (package-name (if (symbolp recipe-id) (symbol-name recipe-id) recipe-id))
               (plist (cdr recipe))
               (target-file (expand-file-name package-name xelpa-recipes-dir)))
          ;; If the recipe uses `:local-repo "."`, resolve it to an
          ;; absolute path to make it unambiguous for straight.el.
          (when (equal (plist-get plist :local-repo) ".")
            (setq plist (plist-put plist :local-repo repo-root)))

          ;; Write the final, processed recipe to its own file in xelpa.
          (with-temp-file target-file
            (prin1 (cons (intern package-name) plist) (current-buffer)))))))
    (message "--- 'xelpa' generation complete ---"))
