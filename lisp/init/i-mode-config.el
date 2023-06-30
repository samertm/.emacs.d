;;; i-mode-config.el --- mode configs -*- lexical-binding: t; -*-

;;; Commentary:

;; Configs for modes and hooks.

;;; Code:

;; set up modes
;; major modes
(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyst\\'" . python-mode))
(setq-default indent-tabs-mode nil
              major-mode 'text-mode)

;; elpy
(elpy-enable)

;; vterm

(setq vterm-max-scrollback 100000)

;; helm

(require 'helm)
(require 'helm-config)
; (helm-mode 0)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-matching t
      helm-M-x-fuzzy-match t
      helm-find-files-ignore-thing-at-point t
      helm-buffer-max-length nil)

(require 'cl-lib)
;; Reorder helm buffers so we use the default order except that the current
;; buffer is placed last. This is different from the standard helm ordering,
;; which places all the active buffers last.
(defun samer-helm-buffers-reorder-helm-buffer-list (visibles others)
  (let ((not-current (cl-loop for b in (buffer-list)
                              for bn = (buffer-name b)
                              unless (equal bn (buffer-name))
                              collect bn)))
    (nconc not-current (list (buffer-name)))))

(setq helm-buffer-list-reorder-fn #'samer-helm-buffers-reorder-helm-buffer-list)

;; make directories on save
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

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


;; eshell
(setq eshell-cmpl-cycle-completions nil)

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
    (setq arguments (append '("--noheading" "--nobreak") arguments))
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
      (setq command-string (concat command-string " | awk -v len=1000 '{ if (length($0) > len) print substr($0, 1, len-3) \"...\"; else print; }'"))
      ;; Call ag.
      (compilation-start
       command-string
       'grep-mode
       `(lambda (mode-name) ,(ag/buffer-name string directory regexp))))))
;; ag.el patch end

;; ag
(setq ag-reuse-buffers t)
(setq ag-ignore-list '("Godeps" "assets" "node_modules" "bower_components" "testdata" "*.map" "*min.js"))

;; scroll
(setq scroll-preserve-screen-position t)

;; projectile-mode

(require 'projectile)
(defun projectile-symbol-at-point () "") ;; turn off this anti-feature.
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action (lambda () (dired (projectile-project-root))))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(require 'helm-projectile)
(defclass helm-source-projectile-buffer (helm-source-sync helm-type-buffer)
  ((init :initform (lambda ()
                     ;; Issue #51 Create the list before `helm-buffer' creation.
                     (setq helm-projectile-buffers-list-cache
                           (ignore-errors (cdr (projectile-project-buffer-names))))
                     (let ((result (cl-loop for b in helm-projectile-buffers-list-cache
                                            maximize (length b) into len-buf
                                            maximize (length (with-current-buffer b
                                                               (symbol-name major-mode)))
                                            into len-mode
                                            finally return (cons len-buf len-mode))))
                       (unless (default-value 'helm-buffer-max-length)
                         (helm-set-local-variable 'helm-buffer-max-length (car result)))
                       (unless (default-value 'helm-buffer-max-len-mode)
                         (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result))))))
   (candidates :initform helm-projectile-buffers-list-cache)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
(helm-projectile-on)
(projectile-global-mode)

;; turn off bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; ag
(setq ag-highlight-search t)

;; popwin-mode
(require 'popwin)
(popwin-mode 1)

;; go-mode
(setq gofmt-command "goimports")

;; go-eldoc
(require 'go-eldoc)

;; company-mode
(setq company-idle-delay nil)

;; save-place
(require 'saveplace)
(setq-default save-place t)

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
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-revision-insert-related-refs nil)


;; show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; uniquify
;; (require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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
(setq js2-basic-offset 2)

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
  (define-key c++-mode-map (kbd "C-c C-n") nil)
  (set (make-local-variable 'company-backends) '(company-capf))
  (setq c-basic-offset 4))

(defun my-java-mode-hook ()
  (c-set-style "java")
  (setq-default c-basic-offset 4))

(defun my-org-mode-hook ()
  (electric-indent-local-mode -1)
  )

(defun my-python-mode-hook ()
  (local-set-key (kbd "<RET>") 'newline-and-indent)
  (local-set-key (kbd "C-j") 'newline)
  ;; (when (or (string= (file-name-extension buffer-file-name) "bzl")
  ;;           (string= buffer-file-name "BUILD"))
  ;;   (setq python-indent 2))
  )

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

(defun my-vterm-mode-hook ()
  (local-unset-key (kbd "C-l")))

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
(add-hook 'vterm-mode-hook 'my-vterm-mode-hook)

(provide 'i-mode-config)
;;; i-mode-config.el ends here
