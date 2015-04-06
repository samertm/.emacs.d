;;; i-utils.el --- my utilities               -*- lexical-binding: t; -*-

;;; Commentary:

;; Functions I've written or stolen from the webz.

;;; Code:

;; load functions
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

;; custom functions

(defun kill-control-block ()
  (interactive)
  (search-backward-regexp "\\(if\\|while\\|for\\|else\\|do\\)")
  (let ((beg (point)))
    (search-forward-regexp "{")
    (delete-region beg (point))
    (search-forward-regexp "}")
    (delete-char -1)
    (indent-region beg (point))
    (goto-char beg)))

(defun console-log-debug ()
  (interactive)) ;do this when I'm less tired @_@

(defun samer-mark-whole-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun samer-mark-line-to-indentation ()
  (interactive)
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun samer-vim-command-s-o ()
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (previous-line nil)
  (indent-according-to-mode))

(defun samer-vim-command-o ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-according-to-mode))

(defun samer-find-file-as-root ()
  "Get file with root privileges.
Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo::" file)))
    (find-file file)))

(defun samer-previous-window ()
  (interactive)
  (other-window -1))

;; from http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(defun samer-kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun samer-new-eshell ()
  (interactive)
  (eshell t))

(defun samer-top-join-line ()
  (interactive)
  (delete-indentation 1))

(defun samer-generate-blog ()
  "Generate my blog."
  (interactive)
  (setq samer-my-shell (start-process "to-blog" "*to blog*" "bash"))
  (process-send-string samer-my-shell "ssh samertm\n")
  (process-send-string samer-my-shell ". virtualenvs/pelican/bin/activate\n")
  (process-send-string samer-my-shell "cd /home/samer/pelican\n")
  (process-send-string samer-my-shell "make html\n")
  (process-send-string samer-my-shell "exit\n")
  (process-send-string samer-my-shell "exit\n")
  (display-buffer "*to blog*" '(display-buffer-pop-up-window . nil)))

(defun src-start-work ()
  "Open up processes needed for work."
  (interactive)
  (process-send-string (shell "*shell*<serve-dev>") "cd $GOPATH/src/sourcegraph.com/sourcegraph/sourcegraph && git pull && make serve-dev\n")
  (process-send-string (shell "*shell*<gulp>") "cd $GOPATH/src/sourcegraph.com/sourcegraph/sourcegraph/app && gulp\n"))


(defun samer-subword-mode-on ()
  (interactive)
  (subword-mode 1))
(defun samer-superword-mode-on ()
  (interactive)
  (superword-mode 1))

;; useful keyboard macros
(fset 'time-set
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217729 19 92 119 40 return 134217826 134217828 67108911 15 116 32 58 61 32 116 105 109 101 46 78 111 119 40 41 return 116 111 116 97 108 32 58 61 32 116 return 108 111 103 46 80 114 105 110 116 108 110 40 34 25 32 84 79 84 65 76 34 44 32 116 105 109 101 46 83 105 110 99 101 40 116 111 116 97 108 41 41 1 102 backspace 100 101 102 101 114 32 102 117 110 99 40 41 33554464 123 32 5 33554464 125 40 41 14 1] 0 "%d")) arg)))

(fset 'time-statement
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 11 67108911 15 108 111 103 46 80 114 105 110 116 108 110 40 34 25 34 backspace 32 84 73 77 69 34 44 32 116 105 109 101 46 83 105 110 99 101 40 116 41 41 return 116 32 61 32 116 105 109 101 46 78 111 119 40 41 14 1] 0 "%d")) arg)))


(provide 'i-utils)
;;; i-utils.el ends here
