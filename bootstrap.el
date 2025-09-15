;; bootstrap.el
;; This script bootstraps straight.el into the init/ directory.
;; It should be run once, before any other CI script.
;; -*- lexical-binding: t -*-

;; all CI scripts must see this value when they are loaded
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
