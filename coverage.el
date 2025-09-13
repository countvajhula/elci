;; coverage.el (Simple Version)
;; This script loads the necessary libraries for coverage testing.
;; It relies on the Makefile to provide the full configuration and commands.
;; -*- lexical-binding: t -*-

(add-to-list 'load-path ".")
(require 'ci)
(ci-load-straight)
(ci-load-optional-deps)
(straight-use-package 'ert-runner)
(straight-use-package 'undercover)

(message "--- Preparing for coverage run ---")

;; Load the main library. Undercover's shutdown hooks, configured
;; by the Makefile, will handle the rest.
(require 'undercover)
(undercover)
