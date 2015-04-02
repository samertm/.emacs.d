;; general config

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

(let ((default-directory "/home/samer/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(load "~/.emacs.d/package-setup" nil t)
(load "~/.emacs.d/custom-functions" nil t)
(load "~/.emacs.d/mode-setup" nil t)
(load "~/.emacs.d/mu4e-config" nil t)
(load "~/.emacs.d/set-keys" nil t)
(load "~/.emacs.d/hooks" nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gh ace-window debbugs vala-mode cider clojure-mode sourcegraph go-stacktracer markdown-mode deft ace-jump-mode magit smex company-go company go-eldoc ag guide-key flycheck flx-ido projectile popwin go-mode ctags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
