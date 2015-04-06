;;; init.el --- init                                 -*- lexical-binding: t; -*-

;;; Commentary:

;; My init file.

;;; Code:

;; suppress gui & startup
(setq inhibit-startup-screen t)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(column-number-mode t)

;; for chromebook
(if (equal system-configuration "armv7l-unknown-linux-gnueabihf")
    (set-face-attribute 'default nil :height 130))

;; Add Lisp stuff.
;; I'm not sure how to do this in a portable way.
(defvar my-lisp-dir "~/.emacs.d/lisp/"
  "The directory with all of my custom Lisp files.")

;; Add my-lisp-dir and subdirectories to load-path.
(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))
(setq custom-file (concat my-lisp-dir "init/custom.el"))

;; Init
(require 'package-setup)
(require 'custom-functions)
(require 'mode-setup)
;(require 'mu4e-config) disable for now.
(require 'set-keys)

;;; init.el ends here
