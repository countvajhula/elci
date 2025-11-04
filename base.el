;; base.el
;; This script provides shared basic utilities.
;; -*- lexical-binding: t -*-

(defun bootstrap-elacarte (recipes-file)
  "Directly read the recipe for elacarte and install it.

RECIPES is a file containing a list of recipes. Reading the recipes.eld
files directly is necessary because a recipe repository that could be
used by Straight doesn't yet exist."
  (unless (file-exists-p recipes-file)
    (user-error "Recipes file %s not found!" recipes-file))
  (let* ((recipes (with-temp-buffer
                    (insert-file-contents-literally elacarte-recipes-file)
                    ;; Use `read-from-string` to handle empty files gracefully.
                    (car (read-from-string (buffer-string)))))
         (recipe (alist-get 'elacarte recipes)))
    (unless recipe
      (user-error "Elacarte recipe missing in %s!" recipes-file))
    (straight-use-package (cons 'elacarte recipe)))
  (setq elacarte-base-dir (expand-file-name "elacarte" "init")))

(provide 'base)
