;; ci.el
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
If non-nil, this indicates the project is a package suite. If nil,
it is treated as a single-package repository.")

;; If a project name is not specified, it cannot be a multi-package repository.
(when (and (null ci-project-name) (> (length ci-packages) 1))
  (error "CI_PROJECT env var must be set for multi-package repositories"))

(defun ci-load-straight ()
  "Load straight.el and register the local 'xelpa' recipe repository."
  ;; All CI scripts must see this value when they are loaded.
  (defvar straight-base-dir (expand-file-name "init"))

  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          straight-base-dir)))
    (unless (file-exists-p bootstrap-file)
      (error "straight.el not found. Run bootstrap.el first."))
    (load bootstrap-file nil 'nomessage))

  ;; Disable recipe inheritance to prevent unnecessary network access.
  (setq straight-allow-recipe-inheritance nil)

  ;; --- Register the 'xelpa' recipe repository ---
  ;; Ensure that straight.el knows about our local recipes and gives
  ;; them the highest priority.
  (let ((xelpa-dir (expand-file-name "xelpa")))
    (when (file-directory-p xelpa-dir)
      (message "--- Registering 'xelpa' recipe repository ---")
      ;; 1. Make the package known to straight.el.
      (straight-use-package `(xelpa :type git :local-repo ,xelpa-dir :build nil))
      ;; 2. Load the recipe protocol implementation.
      (add-to-list 'load-path xelpa-dir)
      (require 'xelpa)
      ;; 3. Add xelpa to the head of the list of repositories to search.
      (add-to-list 'straight-recipe-repositories 'xelpa))))

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
