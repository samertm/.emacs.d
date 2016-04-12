;;; i-mode-config.el --- mode configs -*- lexical-binding: t; -*-

;;; Commentary:

;; Configs for modes and hooks.

;;; Code:

;; set up modes
;; major modes
(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . javascript-mode))
(setq-default indent-tabs-mode nil
              major-mode 'text-mode)

;; elpy
(elpy-enable)

;; helm

(require 'helm)
(require 'helm-config)
(helm-mode 1)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-matching t
      helm-M-x-fuzzy-match t)

;; coffee-mode
(setq coffee-tab-width 2)

;; make directories on save
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; web-mode
;; SAMER: Lazy load web-mode?
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; mail

;; not working
;; (setq send-mail-function    'smtpmail-send-it
;;       smtpmail-smtp-server  "smtp.mailbox.org"
;;       smtpmail-stream-type  'starttls
;;       smtpmail-smtp-service 587)

;; nm (nevermore email client)
(require 'nm-company)

;; fill
;;(setq fill-column 80)
;; The original value is "\f\\|[ \t]*$", so we add the bullets (-), (+), and (*).
;; There is no need for "^" as the regexp is matched at the beginning of line.
;; TODO: get this to work for comments.
;;(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

;; tasklist


;; TODO: enable this & make it global.
;; (use-hard-newlines 1 'never)

;; shell-mode
;; TODO: ask emacs-devel about this.
(add-to-list 'display-buffer-alist '(".*shell.*" . ((display-buffer-same-window) . nil)))

;; whitespace
(setq-default indicate-empty-lines t)
(set-face-attribute 'trailing-whitespace nil
                      :foreground 'unspecified
                      :inverse-video 'unspecified
                      :slant 'unspecified
                      :weight 'unspecified
                      :background "grey")

;; god-mode
;; (require 'god-mode)
;; (defun my-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'box
;;                       'bar)))

;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; eshell
;; (add-to-list 'god-exempt-major-modes 'eshell-mode)
(setq eshell-cmpl-cycle-completions nil)

;; erc
(setq erc-track-enable-keybindings nil)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(setq erc-save-buffer-on-part t)

;; overwrite-mode
(fmakunbound 'overwrite-mode)

;; ag.el patch start
(require 'ag)
(defun ag/dwim-at-point () "") ;; turn off this anti-feature.
(defcustom ag-ignore-list nil
  "A list of patterns to ignore when searching."
  :type '(repeat (string))
  :group 'ag)
(defun ag/format-ignore (ignore)
  "Prepend '--ignore' to every item in IGNORE."
  (let ((result nil))
    (while ignore
      (setq result (append `("--ignore" ,(car ignore)) result))
      (setq ignore (cdr ignore)))
    result))

(require 'cl)
(defun* ag/search (string directory &key (regexp nil) (file-regex nil) (file-type nil))
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments ag-arguments)
        (shell-command-switch "-c"))
    (unless regexp
        (setq arguments (cons "--literal" arguments)))
    (if ag-highlight-search
        (setq arguments (append '("--color" "--color-match" "30;43") arguments))
      (setq arguments (append '("--nocolor") arguments)))
    (when (char-or-string-p file-regex)
      (setq arguments (append `("--file-search-regex" ,file-regex) arguments)))
    (when file-type
      (setq arguments (cons file-type arguments)))
    (when ag-ignore-list
      (setq arguments (append (ag/format-ignore ag-ignore-list) arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (let ((command-string
           (mapconcat 'shell-quote-argument
                      (append (list ag-executable) arguments (list string "."))
                      " ")))
      ;; If we're called with a prefix, let the user modify the command before
      ;; running it. Typically this means they want to pass additional arguments.
      (when current-prefix-arg
        ;; Make a space in the command-string for the user to enter more arguments.
        (setq command-string (ag/replace-first command-string " -- " "  -- "))
        ;; Prompt for the command.
        (let ((adjusted-point (- (length command-string) (length string) 5)))
          (setq command-string
                (read-from-minibuffer "ag command: "
                                      (cons command-string adjusted-point)))))
      ;; Call ag.
      (compilation-start
       command-string
       'grep-mode
       `(lambda (mode-name) ,(ag/buffer-name string directory regexp))))))
;; ag.el patch end

;; ag
(setq ag-reuse-buffers t)
(setq ag-ignore-list '("Godeps" "assets" "node_modules" "bower_components" "testdata"))

;; scroll
(setq scroll-preserve-screen-position t)

;; projectile-mode
(projectile-global-mode)
(defun projectile-symbol-at-point () "") ;; turn off this anti-feature.
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action (lambda () (dired (projectile-project-root))))

;; turn off bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; sourcegraph-mode
;;(require 'sourcegraph nil 'noerror)

;; guide-key
(setq guide-key/guide-key-sequence '("C-c p" "C-x r"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)

;; ag
(setq ag-highlight-search t)

;; popwin-mode
(require 'popwin)
(popwin-mode 1)

;; go-mode.el patch start
(defun govet-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'govet-before-save)."
  ;; (interactive)
  ;; (when (eq major-mode 'go-mode) (govet)))
  nil)

(defun govet ()
  (interactive)
  (compile (concat "go vet " (buffer-file-name))))
;; go-mode.el patch end
;; go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'govet-before-save)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))

;; flycheck-mode
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; go-eldoc
(require 'go-eldoc)

;; company-mode
(setq company-idle-delay nil)

;; smex
(smex-initialize)

;; save-place
(require 'saveplace)
(setq-default save-place t)

;; ido-mode, flx-ido
;; (require 'flx-ido)
;; ;; By default, ido does not have flex matching enabled.
;; (setq ido-enable-flex-matching t
;;       ido-everywhere t
;;       ;; By default, ido-mode will change your directory if you type
;;       ;; the name of a file that doesn't exist. Set
;;       ;; ido-auto-merge-work-directories-length to a negative number
;;       ;; to disable that behavior.
;;       ido-auto-merge-work-directories-length -1
;;       ;; By default, ido-mode will ask you if you want to create a new
;;       ;; buffer when you type the name of a buffer that doesn't exist.
;;       ;; Set ido-create-new-buffer to always to always create a new
;;       ;; buffer.
;;       ido-create-new-buffer 'always
;;       ;; By default, ido-mode will open a file in the selected window
;;       ;; *unless* that file is open in another frame, in which case it
;;       ;; will simply raise that frame. Set ido-default-file-method to
;;       ;; 'selected-window to *always* open a file in the selected
;;       ;; window.
;;       ido-default-file-method 'selected-window
;;       ;; ido-default-buffer-method has the same behavior as
;;       ;; ido-default-file-method by default.
;;       ido-default-buffer-method 'selected-window
;;       ido-max-directory-size 100000)
;; (ido-mode 1) ;; TODO: (ido-mode 'both) ?
;; (flx-ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; org-mode
(setq org-log-done 'time)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/planner.org"))
(setq org-default-notes-file "~/org/refile.org")
(setq initial-buffer-choice "~/org/notes.org")
(setq org-todo-keywords '("TODO(t)" "NEXT(n)" "SOMEDAY(s)" "WAITING(w)" "DONE(d)"))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))
                           ("~/org/notes.org" . (:maxlevel . 3))))
(setq org-completion-use-ido t)

;; tasklist
(require 'tasklist nil t) ;; no error
(setq tasklist-directory-name (expand-file-name "tasks" org-directory))
(setq tasklist-auto-insert"* tasks
** do
** small
** sessions")

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; uniquify
;; (require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; set electric-indent-mode in <= emacs 24.3
; (electric-indent-mode 1)

;; ctags
(require 'ctags)
(setq tags-revert-without-query t)

;; misc config
(fmakunbound 'suspend-frame)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq apropos-do-all t
      sentence-end-double-space nil
      ;; scroll
      scroll-margin 4
      scroll-conservatively 1

      create-lockfiles nil

      mouse-yank-at-point t
      ;; saving
      auto-save-default nil
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

;; js-mode
(setq js-indent-level 2)

;; Set up hooks.

(defun my-prog-mode-hook ()
  (setq-default indent-tabs-mode nil)
  ;(sourcegraph-mode 1)
  (local-set-key (kbd "C-a") 'back-to-indentation)
  (local-set-key (kbd "M-m") 'move-beginning-of-line)
  (setq show-trailing-whitespace t))

(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq-default c-basic-offset 2))

(defun my-c++-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 4))

(defun my-java-mode-hook ()
  (c-set-style "java")
  (setq-default c-basic-offset 4))

(defun my-org-mode-hook ()
  ;;(org-indent-mode 1)
  )

(defun my-python-mode-hook ()
  (local-set-key (kbd "<RET>") 'newline-and-indent)
  (local-set-key (kbd "C-j") 'newline))

(defun my-scheme-mode-hook ()
  ;(enable-paredit-mode))
  )

(defun my-emacs-lisp-mode-hook ()
  ;(enable-paredit-mode))
  )

(defun my-go-mode-hook ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (company-mode 1)
  (set (make-local-variable 'company-backends) '(company-go)))

(defun my-php-mode-hook ()
  (setq c-basic-offset 4)
  (php-enable-pear-coding-style))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'php-mode-hook 'my-php-mode-hook)

(provide 'i-mode-config)
;;; i-mode-config.el ends here
