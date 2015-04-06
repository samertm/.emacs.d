;;; init.el --- init                                 -*- lexical-binding: t; -*-

;;; Commentary:

;; My init file.

;;; Code:

;; Suppress GUI & startup.
(setq inhibit-startup-screen t)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(column-number-mode t)

;; For chromebook
(if (equal system-configuration "armv7l-unknown-linux-gnueabihf")
    (set-face-attribute 'default nil :height 130))

;; Add Lisp dir to loadpath..
(defvar my-lisp-dir "/home/samer/.emacs.d/lisp/" ; TODO: make portable.
  "The directory with all of my custom Lisp files.")

(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))
(setq custom-file (concat my-lisp-dir "init/custom.el"))

;; Call my Emacs init functions.
(require 'i-package-setup)
(require 'i-utils)
(require 'i-mode-config)
(require 'i-keys)
(require 'i-custom)

;;; init.el ends here
