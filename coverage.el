;; coverage.el (Simple Version)
;; This script loads the necessary libraries for coverage testing.
;; It relies on the Makefile to provide the full configuration and commands.
;; -*- lexical-binding: t -*-

;; Add the current directory (emacs-ci/) to the load-path.
(add-to-list 'load-path ".")

;; Load shared helpers and dependencies.
(require 'ci)
(ci-load-straight)
(ci-load-optional-deps)

;; Install the necessary dependencies for testing and coverage.
(straight-use-package 'ert-runner)
(straight-use-package 'undercover)

(message "--- Preparing for coverage run ---")

;; Load the main undercover library. Its shutdown hooks, configured by the
;; Makefile's environment variables, will handle the report generation.
(require 'undercover)
(undercover)

