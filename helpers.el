;; helpers.el
;; This script provides shared functions and constants for the CI process.
;; -*- lexical-binding: t -*-

(defconst ci-packages
  (let ((packages-env (getenv "CI_PACKAGES")))
    (unless (and packages-env (not (string-equal packages-env "")))
      (error "The CI_PACKAGES environment variable is not set or is empty."))
    (split-string packages-env " " t))
  "A list of all packages to be checked in the CI process.")

(defconst ci-project-name (getenv "CI_PROJECT")
  "The project name, read from the CI_PROJECT environment variable.
If non-nil, this indicates the project is a package suite, where
each package's source files are in a subdirectory named after the
package. If nil, it is treated as a single-package repository.")

;; If a project name is not specified, it cannot be a multi-package repository.
(when (and (null ci-project-name) (> (length ci-packages) 1))
  (error "CI_PROJECT env var must be set for multi-package repositories"))


(defun ci-load-straight ()
  "Load the straight.el library from the local CI installation.
This must be called at the beginning of any script that
needs to use straight.el's functions."
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (expand-file-name "ci-init"))))
    (unless (file-exists-p bootstrap-file)
      (error "straight.el not found. Run install.el first."))
    (load bootstrap-file nil 'nomessage)
    ;; ensure all CI scripts use the local init path
    (setq straight-base-dir (expand-file-name "ci-init"))))

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
  "Return absolute paths to all .el files for PKG-NAME, including subdirs."
  (directory-files-recursively (straight--build-dir pkg-name) "\\.el$"))

(defun ci-get-package-source-files (pkg-name)
  "Return absolute paths to the source .el files for PKG-NAME.
This uses the straight.el build directory and excludes generated files."
  (let* ((all-files (ci-get-package-all-files pkg-name)))
    ;; Exclude generated autoload files from checks.
    (cl-remove-if (lambda (file) (string-match-p "-autoloads\\.el$" file))
                  all-files)))

(provide 'helpers)
