;;; writer-tests-utils.el --- Utilities for tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019-2020 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience files wp
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
;; Utilities for tests
;; See CONTRIBUTING.org for documentation on how to run these tests.

;;; Code:

(require 'writer)

(defmacro writer-tests-utils-visit-fixture (name &rest init)
  "Visit the fixture NAME and pass an INIT block before doing so."

  (declare (indent 0) (debug t))
  `(let ((path (expand-file-name (concat ,default-directory "t/fixtures/" ,name))))
     (if (file-exists-p path)
         (progn
           (unless noninteractive
             (writer-delete-other-windows-and-clones)
             (kill-this-buffer))
           ,@init
           (find-file path)
           (unless noninteractive
             (writer-mode 1)))
       (error "Fixture '%s' does not exist" path))))

;; This function has been taken from: https://stackoverflow.com/a/1511827
(defun writer-tests-utils-which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."

  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    active-modes))

(defun writer-tests-utils-more-or-less-p (smaller bigger proportion)
  "Whether BIGGER is more or less PROPORTION times bigger than SMALLER."

  (let ((prod (* smaller proportion)))
    (and (> bigger (- prod 10)) (< bigger (+ prod 10)))))

(provide 'writer-tests-utils)

;;; writer-tests-utils.el ends here
