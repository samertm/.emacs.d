;; general config

;; suppress gui & startup
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)

;; for chromebook
(if (equal system-configuration "armv7l-unknown-linux-gnueabihf")
    (set-face-attribute 'default nil :height 130))

(setq initial-buffer-choice 'remember-notes)

(let ((default-directory "~/.emacs.d/lisp/"))
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
 '(erc-modules
   (quote
    (completion netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "blue4"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))
(put 'upcase-region 'disabled nil)
