;;; tasklist.el --- manage tasks                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Samer Masterson

;; Author: Samer Masterson <samer@samertm.com>
;; Keywords: outlines

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage your tasks. Call `tasklist-open-tasklist'.

;;; Code:

(defgroup tasklist nil
  ""                                    ; TODO: fill out doc string.
  :group 'outlines)

(defcustom tasklist-directory-name
  (locate-user-emacs-file "tasklist")
  "The directory where tasklists are kept."
  :type 'directory
  :group 'tasklist)

(defcustom tasklist-file-suffix "-tasklist.org"
  "The file suffix for tasklists."
  :type 'string
  :group 'tasklist)

;; TODO:
;;  - decide on a good format for the dates.
;;  - provide a date format conversion function to convert files from
;;  one to the other.
(defcustom tasklist-time-format "%m-%d-%Y"
  "The time format string for tasklist files."
  :type 'string
  :group 'tasklist)

;; FIXME: I think lisp-2s are bad (and tasklist-auto-insert seems like
;; it should be a function as well), but it's just so convenient. I
;; can't resist!
(defcustom tasklist-auto-insert "* tasks\n"
  "The string to insert into new tasklist files."
  :type 'string
  :group 'tasklist)

;;;###autoload
(defun tasklist-open-tasklist (&optional time)
  "Open the tasklist for TIME, or today if omitted.
Switch to a buffer visiting the tasklist, creating it if it
doesn't exist.
Creates directory `tasklist-directory-name' if it doesn't exist."
  (interactive)
  (make-directory tasklist-directory-name t) ; Always attempt to create directory.
  (find-file (tasklist-file-path time))
  ;; If the file is new, insert `tasklist-auto-insert-string'.
  (when (string= (buffer-string) "")    ; FIXME: incorrect when narrowing is in effect.
    (insert tasklist-auto-insert)))

(defun tasklist-file-path (&optional time)
  "Return absolute file path for the tasklist at TIME.
If TIME is nil, return the file path for today."
  (expand-file-name
   (concat (format-time-string "%m-%d-%Y" (if time time))
           tasklist-file-suffix)
   tasklist-directory-name))

(provide 'tasklist)
;;; tasklist.el ends here
