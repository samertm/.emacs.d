;; set up modes
;; major modes
(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . javascript-mode))
(setq-default indent-tabs-mode nil
              major-mode 'text-mode)

;; erc
(setq erc-track-enable-keybindings nil)

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
(setq ag-ignore-list '("Godeps" "assets" "node_modules" "bower_components"))

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

;; go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))

;; flycheck-mode
(add-hook 'after-init-hook #'global-flycheck-mode)

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
(require 'flx-ido)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil) 

;; org-mode
(setq org-log-done 'time)
(setq org-directory "~/org/")

;; show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; deft
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/org")

;; uniquify
;; (require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; set electric-indent-mode in <= emacs 24.3
; (electric-indent-mode 1)

;; ctags
(require 'ctags)
(setq tags-revert-without-query t)

;; misc config
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

