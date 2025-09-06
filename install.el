;; install.el
;; This script is the main entry point for installing all packages.
;; It loads external dependency definitions and then installs the project's
;; own packages. It must be run *after* bootstrap.el.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path so we can `require`
;; other generic scripts like helpers.el.
(add-to-list 'load-path ".")

;; Load the shared CI helper functions and constants.
(require 'helpers)
(ci-load-straight)

;; Optionally load project-specific external dependencies from the project's
;; own `ci/` directory, which is one level up from this script's location.
(let ((project-ci-dir (expand-file-name "../ci")))
  (when (file-exists-p (expand-file-name "ci-deps.el" project-ci-dir))
    (add-to-list 'load-path project-ci-dir)
    (require 'ci-deps)))

(setq straight-allow-recipe-inheritance nil)


;; --- Install project packages ---
(message "--- Installing project packages ---")

(let ((repo-root (expand-file-name ".."))
      (is-suite ci-project-name))
  (dolist (pkg ci-packages)
    (let* ((relative-dir (if is-suite pkg "."))
           ;; For a package suite, each package is in a subdir named after it.
           ;; For single-package repos, files are at the root.
           (source-dir (expand-file-name relative-dir repo-root))
           ;; Manually expand the glob into a list of files. The paths
           ;; must be relative to the repo root for the :files keyword.
           (files (mapcar (lambda (file) (file-relative-name file repo-root))
                          (directory-files source-dir t "\\.el$"))))

      (straight-use-package
       `(,(intern pkg) :local-repo ,repo-root :files ,files)))))

(message "--- Package installation complete ---")
