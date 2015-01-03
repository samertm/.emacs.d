;; swiped from http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html
(when (require 'mu4e nil t) ;; mu4e is optional

  (setq mu4e-hide-index-messages t)

  ;; default
  ;; (setq mu4e-maildir "~/Maildir")

  (setq mu4e-drafts-folder "/nosefrog/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/nosefrog/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/nosefrog/[Gmail].Trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '(("/nosefrog/INBOX"    . ?n)
          ("/sourcegraph/INBOX" . ?s)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300)

  ;; something about ourselves
  (setq
   user-mail-address "nosefrog@gmail.com"
   user-full-name  "Samer Masterson"
   mu4e-compose-signature
   (concat
    "    Samer\n"))

  ;; sending mail -- replace USERNAME with your gmail username
  ;; also, make sure the gnutls command line utils are installed
  ;; package 'gnutls-bin' in Debian/Ubuntu

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "nosefrog@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  ;; alternatively, for emacs-24 you can use:
  ;;(setq message-send-mail-function 'smtpmail-send-it
  ;;     smtpmail-stream-type 'starttls
  ;;     smtpmail-default-smtp-server "smtp.gmail.com"
  ;;     smtpmail-smtp-server "smtp.gmail.com"
  ;;     smtpmail-smtp-service 587)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t
        mu4e-use-fancy-chars t))
