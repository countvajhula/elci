;; ci.el
;; This script provides shared functions and constants for the CI process.
;; -*- lexical-binding: t -*-

(require 'base)

(defun ci-load-straight ()
  "Load straight.el and register the local Elacarte recipe repository."
  ;; All CI scripts must see this value when they are loaded.
  (defvar straight-base-dir (expand-file-name "init"))

  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          straight-base-dir)))
    (unless (file-exists-p bootstrap-file)
      (error "straight.el not found. Run bootstrap.el first."))
    (load bootstrap-file nil 'nomessage))

  (require 'straight)

  ;; Disable recipe inheritance to prevent unnecessary network access.
  (setq straight-allow-recipe-inheritance nil)

  ;; Install Elacarte.
  ;; This ensures that straight.el knows about our local recipes and gives
  ;; them the highest priority.
  (let ((elci-recipes (expand-file-name "ci-recipes.eld")))
    ;; Ordinarily, as Elacarte is a dependency of Elci, convention
    ;; would dictate including its recipe as a "pointer" in Elci's
    ;; main cookbook (i.e. `recipes.eld`), where it is indeed present.
    ;; But since Elacarte itself is needed in order to be able to
    ;; follow such a pointer and discover the canonical recipe, we
    ;; would need to duplicate this algorithm here in order for this
    ;; to work correctly. Instead, it's simpler to just duplicate the
    ;; canonical recipe in a special `ci-recipes.eld` file that is
    ;; used literally rather than traversed.
    ;; This is only needed in case of a direct dependency on Elacarte
    ;; itself.
    (bootstrap-elacarte elci-recipes)
    (require 'elacarte)
    ;; Rebuild the cookbook so straight knows about it.
    (elacarte-use-cookbook)))

(ci-load-straight)

(defconst ci-packages
  (or
   ;; use environment variable if set. This may be useful in cases
   ;; where the local repo name on the filesystem differs from the
   ;; repo name on the host, where the implicit inference of packages
   ;; provided by the project repo would fail to detect primary
   ;; recipes.
   (let ((packages-env (getenv "CI_PACKAGES")))
     (when (and packages-env (not (string-equal packages-env "")))
       (split-string packages-env " " t)))
   (let ((repo-root (expand-file-name "..")))
     (mapcar #'elacarte--package-name
             (elacarte-get-primary-recipes
              (elacarte--pointer-recipe-for-url repo-root)))))
  "A list of all packages to be checked in the CI process.")

(defconst ci-project-name (getenv "CI_PROJECT")
  "The project name, read from the CI_PROJECT environment variable.
If non-nil, this indicates the project is a package suite. If nil,
it is treated as a single-package repository.")

;; If a project name is not specified, it cannot be a multi-package repository.
(when (and (null ci-project-name) (> (length ci-packages) 1))
  (error "CI_PROJECT env var must be set for multi-package repositories"))

(defun ci-get-load-path-args (pkg-name &optional extra-dirs)
  "Return a list of \"-L /path\" arguments for PKG-NAME.
This includes the package's build directory and all its dependencies.
Optional EXTRA-DIRS can be provided to add more directories to the path."
  (let* ((build-dir (straight--build-dir pkg-name))
         (deps-dirs (mapcar #'straight--build-dir
                            (straight--flatten (straight-dependencies pkg-name))))
         (all-dirs (append extra-dirs deps-dirs (list build-dir))))
    (mapcan (lambda (dir) (list "-L" dir)) all-dirs)))

(defun ci-get-package-main-file (pkg-name)
  "Return the absolute path to the main .el file for PKG-NAME."
  (expand-file-name (concat pkg-name ".el") (straight--build-dir pkg-name)))

(defun ci-get-package-all-files (pkg-name)
  "Return absolute paths to all .el files for PKG-NAME."
  (directory-files-recursively (straight--build-dir pkg-name) "\\.el$"))

(defun ci-get-package-source-files (pkg-name)
  "Return absolute paths to the source .el files for PKG-NAME,
excluding generated autoloads."
  (let* ((all-files (ci-get-package-all-files pkg-name)))
    (cl-remove-if (lambda (file) (string-match-p "-autoloads\\.el$" file))
                  all-files)))

(provide 'ci)
