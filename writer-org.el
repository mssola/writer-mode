;;; writer-org.el --- Further org-mode integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.1"))
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

;;; Utils

;; Function taken from https://emacs.stackexchange.com/a/21472.
(defun writer-org--global-props (&optional buffer keyword)
  "Get the plists of global org properties from the given BUFFER.
If not buffer was passed, then this function will call `current-buffer'.
Moreover, you can pass the KEYWORD to be used for the search.  It defaults to
'PROPERTY' if no keyword was specified."

  (with-current-buffer (or buffer (current-buffer))
    (org-element-map
        (org-element-parse-buffer)
        'keyword
      (lambda (el)
        (when (string-equal (org-element-property :key el) (or keyword "PROPERTY"))
          (nth 1 el))))))

(defun writer-org--prop-value (key)
  "Fetch the value of the property by the given KEY."

  (let ((key-re (writer-org--key-re key))
        (props (writer-org--global-props))
        ret
        val)
    (dolist (pr props)
      (save-match-data
        (and (string-match key-re (setq val (plist-get pr :value))))
        (setq ret (match-string 1 val))))
    ret))

(defun writer-org--key-re (key)
  "Fetch a regular expression for properties by the given KEY."

  (concat "^" (regexp-quote key) ":\\(\\w+\\)"))

;;; Enabling writer-mode

(defun writer-org-enable-p ()
  "Return non-nil if 'writer-mode' is set to 't' in an org property."

  (let ((enable (writer-org--prop-value "writer-mode")))
    (when (string= enable "t") t)))

(provide 'writer-org)

;;; writer-org.el ends here
