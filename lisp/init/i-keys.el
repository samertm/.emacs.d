;;; i-keys.el --- set key bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Set key bindings.

;;; Code:

;; unset keys
;; unset news keys (guilty as charged, I am the fat-fingered man.)
(global-unset-key (kbd "C-h C-n"))
(global-unset-key (kbd "C-h n"))

;; set keys
(global-set-key (kbd "C-c C-a") 'samer-mark-line-to-indentation)
(global-set-key (kbd "C-c M-m") 'samer-mark-whole-line)
(global-set-key (kbd "C-M-o") 'samer-vim-command-s-o)
(global-set-key (kbd "C-o") 'samer-vim-command-o)
(global-set-key (kbd "C-x F") 'samer-find-file-as-root)
(global-set-key (kbd "C-x O") 'samer-previous-window)
(global-set-key (kbd "C-x k") 'samer-kill-this-buffer)
(global-set-key (kbd "C-^") 'samer-top-join-line)

;; helm bindings
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)


;; Org-mode bindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-o")
                (lambda () (interactive) (find-file "~/org/planner.org")))

(global-set-key (kbd "C-c t") 'tasklist-open-tasklist)

;; Eval keybindings (TODO: add more)
(global-set-key (kbd "C-c e b") 'eval-buffer)

(global-set-key (kbd "C-h C-f") 'find-function)

(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-c o") 'ace-window)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
;; (global-set-key (kbd "<insert>") 'god-local-mode)
;; (global-set-key (kbd "<escape>") 'god-local-mode)

;; (global-set-key (kbd "C-c o") 'mu4e)

(global-set-key (kbd "C-c C-M-x") 'edebug-defun)

(global-set-key (kbd "C-c u") 'samer-subword-mode-on)
(global-set-key (kbd "C-c i") 'samer-superword-mode-on)

;; so fat
(global-set-key (kbd "C-c x C-o") 'other-frame)
(global-set-key (kbd "C-c x o") 'other-frame)
(global-set-key (kbd "C-x f") 'ido-find-file)

;; fat-finger
(global-set-key (kbd "C-x C-r") 'eval-last-sexp)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)

(global-set-key (kbd "C-c C-r") 'recompile)

(global-set-key (kbd "C-c C-s C-M-s") 'hs-show-all)
(global-set-key (kbd "C-c C-s C-M-h") 'hs-hide-all)
(global-set-key (kbd "C-c C-s C-c") 'hs-toggle-hiding)
(global-set-key (kbd "C-c C-s C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s C-l") 'hs-hide-level)
(global-set-key (kbd "C-c C-s C-s") 'hs-show-block)

(global-set-key (kbd "C-c C-m") 'make-directory)

(global-set-key (kbd "C-c M-n") 'company-complete)
(global-set-key (kbd "C-c C-n") 'company-complete)

;(global-set-key (kbd "M-x") 'smex)
;; TODO: Find the equivalent w/ helm.
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "<f8>") 'deft)

(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-m") 'eshell)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") 'samer-new-eshell)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-d") 'ido-dired)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'shell-command)

(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c q r") 'query-replace-regexp)

(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

(global-set-key (kbd "C-c C-j") 'imenu)

;; make C-x b and C-x C-b the same
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; exchange zap-to-char for zap-up-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'zap-to-char)

;; ctags
(global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)
(global-set-key (kbd "M-.")  'ctags-search)

(if (< emacs-major-version 24)
    (global-set-key (kbd "RET") 'newline-and-indent)) ; in place of electric-indent

;; windmove keys
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window-if-larger-than-buffer)

(provide 'i-keys)
;;; i-keys.el ends here
