;; bootstrap.el
;; This script bootstraps straight.el and generates the local recipe repo.
;; It should be run once, before any other CI script.
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


;; --- Generate the local 'xelpa' recipe repository ---
(let* ((repo-root (expand-file-name ".."))
       (project-recipe-dir (expand-file-name ".ci/recipes" repo-root))
       (xelpa-dir (expand-file-name "xelpa"))
       (xelpa-recipes-dir (expand-file-name "recipes" xelpa-dir)))

  (when (file-directory-p project-recipe-dir)
    (message "--- Generating local 'xelpa' recipe repository ---")
    ;; Ensure the target directory is clean and exists.
    (when (file-directory-p xelpa-recipes-dir)
      (delete-directory xelpa-recipes-dir t))
    (make-directory xelpa-recipes-dir t)

    ;; Copy and process each user-provided recipe.
    ;; Filter the list to include only regular files, excluding directories.
    (dolist (recipe-file (cl-remove-if #'file-directory-p
                                       (directory-files project-recipe-dir t)))
      (let* ((package-name (file-name-nondirectory recipe-file))
             (target-file (expand-file-name package-name xelpa-recipes-dir))
             (recipe (with-temp-buffer
                       (insert-file-contents recipe-file)
                       (read (current-buffer))))
             ;; Separate the package name from its plist for safe modification.
             (pkg (car recipe))
             (plist (cdr recipe)))
        ;; Resolve the `:local-repo "."` to an absolute path.
        (when (equal (plist-get plist :local-repo) ".")
          ;; Use plist-put to safely modify the property list.
          (setq plist (plist-put plist :local-repo repo-root)))
        ;; Write the reconstructed, valid recipe to the xelpa directory.
        (with-temp-file target-file
          (prin1 (cons pkg plist) (current-buffer)))))
    (message "--- 'xelpa' generation complete ---")))
