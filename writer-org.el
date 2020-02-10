;;; writer-org.el --- Further org-mode integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019-2020 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, files, wp
;; URL: https://github.com/mssola/writer-mode
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file provides additional integration capabilities with org-mode.
;; See README.org for documentation on how to use this minor mode.

;;; Code:

(require 'org-element)

;;; Org utils

;; Function taken from https://emacs.stackexchange.com/a/21472.
(defun writer-org--global-props (&optional buffer keyword)
  "Get the plists of global org properties from the given BUFFER.
If BUFFER was not passed, then this function will call `current-buffer'.
Moreover, you can pass the KEYWORD to be used for the search.  It defaults to
'PROPERTY' if no keyword was specified."

  (with-current-buffer (or buffer (current-buffer))
    (org-element-map
        (org-element-parse-buffer)
        'keyword
      (lambda (el)
        (when (string-equal (org-element-property :key el) (or keyword "PROPERTY"))
          (nth 1 el))))))

(defun writer-org--prop-value (key &optional buffer)
  "Fetch the value of the property by the given KEY from the given BUFFER.
If BUFFER was not passed, then the current buffer is assumed."

  (let ((key-re (writer-org--key-re key))
        (props (writer-org--global-props buffer))
        ret
        val)
    (dolist (pr props)
      (save-match-data
        (and (string-match key-re (setq val (plist-get pr :value)))
             (setq ret (match-string 1 val)))))
    ret))

(defun writer-org--key-re (key)
  "Fetch a regular expression for properties by the given KEY."

  (concat "^" (regexp-quote key) ":\\(\\w+\\)"))

;;; Finding the `writer-main-file' property on a directory

(defun writer-org--find-main-tag-in-buffer (buffer)
  "Get the file name if the `writer-main-file' property exists in BUFFER."

  (let ((main-file (writer-org--prop-value "writer-main-file" buffer))
        (file-name (buffer-file-name buffer)))
    (when (string= main-file "t") file-name)))

(defun writer-org--call-function-buffer-file (fn file)
  "Call the given FN function with the FILE's buffer as its argument.
If the given file did not have an open buffer, then it will be visitted and
killed when we are done.  This function returns the return value of FN."

  (let ((buffer (find-buffer-visiting file))
        ret)

    (if buffer
        (setq ret (funcall fn buffer))
      (save-excursion
        (setq buffer (find-file-noselect file))
        (setq ret (funcall fn buffer))
        (kill-buffer buffer)))
    ret))

(defun writer-org--find-main-file ()
  "Get the name of the main file of the project."

  (let ((list (directory-files default-directory t ".org$"))
        file)

    ;; From the list of org files of the current directory, find the one with
    ;; the `writer-main-file' property. If this is found, then `file' will be set to
    ;; the name of the file, otherwise it will remain nil.
    (while list
      (setq file (writer-org--call-function-buffer-file
                  'writer-org--find-main-tag-in-buffer
                  (car list)))
      (if file
          (setq list nil)
        (setq list (cdr list))))

    ;; Return the found file if this is the case. Otherwise return the file of
    ;; the current buffer.
    (if file
        file
      (buffer-file-name (current-buffer)))))

;;; Exporting to PDF and ODT.

(defun writer-org--move-file-to-out (dir name ext)
  "Move out from DIR the given file NAME if it exists.
Note that EXT must include the period character."

  (when (file-exists-p (concat dir name ext))
    (unless (file-directory-p (concat dir "out/"))
      (make-directory (concat dir "out/")))
    (when (file-exists-p (concat dir "out/" name ext))
      (delete-file (concat dir "out/" name ext)))
    (rename-file (concat dir name ext) (concat dir "out/" name ext))))

(defun writer-org--export-buffer-to-pdf (buffer)
  "Export the given BUFFER to PDF format."

  (with-current-buffer buffer
    (org-latex-export-to-pdf)

    (let* ((fn (buffer-file-name buffer))
           (dir (file-name-directory fn))
           (name (file-name-base fn))
           (autodir (concat dir "auto")))

      (when (file-directory-p autodir)
        (delete-directory autodir t))

      (when (file-exists-p (concat dir name ".tex"))
        (delete-file (concat dir name ".tex")))

      (writer-org--move-file-to-out dir name ".pdf"))))

(defun writer-org--export-buffer-to-odt (buffer)
  "Export the given BUFFER to ODT format."

  (with-current-buffer buffer
    (org-odt-export-to-odt)

    (let* ((fn (buffer-file-name buffer))
           (dir (file-name-directory fn))
           (name (file-name-base fn)))

      (writer-org--move-file-to-out dir name ".odt"))))

(defun writer-org--export (fn)
  "Export to a different format by using FN.
This wrapper is important because it will pick the proper buffer to be
exported.  This is done by assuming that there is a `writer-main-file' property
in a file from the current directory.  If this not the case, then the current
buffer is picked."

  (let ((file (writer-org--find-main-file)))
    (writer-org--call-function-buffer-file fn file)))

(defun writer-org-export-to-pdf ()
  "Export the project to PDF format."

  (interactive)
  (writer-org--export 'writer-org--export-buffer-to-pdf))

(defun writer-org-export-to-odt ()
  "Export the project to ODT format."

  (interactive)
  (writer-org--export 'writer-org--export-buffer-to-odt))

;;; Enabling writer-mode

(defun writer-org-enable-p ()
  "Return non-nil if 'writer-mode' is set to 't' in an org property."

  (let ((enable (writer-org--prop-value "writer-mode")))
    (when (string= enable "t") t)))

(provide 'writer-org)

;;; writer-org.el ends here
