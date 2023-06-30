;;; i-package-setup.el --- package setup               -*- lexical-binding: t; -*-

;;; Commentary:

;; Set up packages.

;;; Code:


(require 'package)

;; custom package-specific functions
(defun samer-get-packages (packages-list)
  (let ((refreshed nil))
    (while (car packages-list)
      (if (not (package-installed-p (car packages-list)))
          (if (not refreshed)
              (progn (package-refresh-contents)
                     (setq refreshed t))
          (package-install (car packages-list)))
      (setq packages-list (cdr packages-list))))))

;; set up package repos
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; install packages
(setq packages '(
                 ace-jump-mode
                 ace-window
                 ag
                 go-eldoc
                 company
                 company-go
                 elpy
                 exec-path-from-shell
                 flx-ido
                 flycheck
                 go-eldoc
                 go-mode
                 helm
                 helm-projectile
                 magit
                 php-mode
                 popwin
                 projectile
                 rjsx-mode
                 ))

;; hotfix

;; (if (functionp 'package--mapc)
;;     (progn
;;       (defun package-read-all-archive-contents ()
;;         "Re-read `archive-contents', if it exists.
;; If successful, set `package-archive-contents'."
;;         (setq package-archive-contents nil)
;;         (dolist (archive package-archives)
;;           (package-read-archive-contents (car archive)))
;;         ;; Build compat table.
;;         (if package--initialized
;;             (progn
;;               (setq package--compatibility-table (make-hash-table :test 'eq))
;;               (package--mapc #'package--add-to-compatibility-table))))))


;; NOTE: after 25.0 this may not be needed.
(package-initialize t) ; read packages without loading
(samer-get-packages packages)
(package-initialize)

(provide 'i-package-setup)
;;; i-package-setup.el ends here
