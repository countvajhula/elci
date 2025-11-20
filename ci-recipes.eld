;; This file contains default recipes for use during CI, either in the
;; bootstrapping phase of setting up Elci itself or for any CI
;; operations on client repos.
;;
;; It only needs to include recipes that are not available on package
;; archives (e.g., ELPA) that can be expected to be configured in the
;; client's package manager, or which are intended to override such
;; recipes. During CI, the recipes in this file have the lowest
;; precedence, and may be overridden both by the project cookbook
;; (`recipes.eld`) and the project's CI cookbook (`.ci/recipes.eld`).
((elacarte
  :host github
  :repo "countvajhula/elacarte"
  :type git
  :files ("*.el" "recipe-repo.el.template")))
