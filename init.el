;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; Add Lisp dir to loadpath.
(defvar my-lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "The directory with all of my custom Lisp files.")

(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Set custom file.
(let ((init-dir (expand-file-name "init" my-lisp-dir)))
  (setq custom-file (expand-file-name "i-custom.el" init-dir)))

;; Call my Emacs init functions. The "i" prefix means both init and
;; "I" in the Rastafari sense.
(require 'i-package-setup)
(require 'i-utils)
(require 'i-mode-config)
(require 'i-keys)
(require 'i-custom)

;;; init.el ends here
