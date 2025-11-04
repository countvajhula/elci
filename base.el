;; base.el
;; This script provides shared basic utilities.
;; -*- lexical-binding: t -*-

(defun bootstrap-elacarte (recipes-file)
  "Directly read the recipe for elacarte and install it.

RECIPES is a file containing a list of recipes. Reading the recipes.el
files directly is necessary because a recipe repository that could be
used by Straight doesn't yet exist."
  (let* ((recipes (when (file-exists-p recipes-file)
                    (with-temp-buffer
                      (insert-file-contents-literally recipes-file)
                      (read (current-buffer)))))
         (elacarte-recipe (cons 'elacarte (alist-get 'elacarte recipes))))
    (straight-use-package elacarte-recipe))
  (setq elacarte-base-dir (expand-file-name "elacarte" "init")))

(provide 'base)
