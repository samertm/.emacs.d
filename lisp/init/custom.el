;;; custom.el --- custom vars -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom stuff gets dumped here.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gh ace-window debbugs vala-mode cider clojure-mode sourcegraph markdown-mode deft ace-jump-mode magit smex company-go company go-eldoc ag guide-key flycheck flx-ido projectile popwin go-mode ctags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

(provide 'init-custom)
;;; custom.el ends here
