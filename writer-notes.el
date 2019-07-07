;;; writer-mode --- A minor mode for writers.

;; Copyright (C) 2019 Miquel Sabaté Solà <mikisabate@gmail.com>
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

;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Keywords: minor mode writer
;; URL: https://github.com/mssola/writer-mode

;;; Commentary:
;;
;; Code for handling the notes section.

;;; Code:

(defcustom writer-notes-post-hook nil
  "Hook run just *after* setting up the notes section.
This hook is called without passing any argument.  Use this hook
to further tune the look and feel of the right section (e.g. opening up a new
window below with dired mode in it)."
  :type 'hook
  :group 'writer)

(defun writer-notes-create-workspace ()
  "Create the environment from scratch.
For that, it assumes that there is only one window on the frame, and it then
allocates a 1/4 on the right for the notes section."

  (dotimes (i 3)
    (split-window-right)
    (windmove-right))

  (writer-notes-init)
  (run-hook-with-args writer-notes-post-hook)
  (balance-windows)

  (dotimes (i 2)
    (windmove-left)
    (delete-window))
  (windmove-left))

(defun writer-notes-init ()
  "Allocate the contents of the notes section."

  (find-file "notes.org" t))

(defun writer-notes-init-and-balance ()
  "Initialize the notes space and balance the environment.
As for the environment, it assumes that there are 4 opened
vertical windows, the rightmost of which corresponds to the
current one."

  (writer-notes-init)
  (balance-windows)
  (windmove-left)
  (delete-window)
  (windmove-left))

(provide 'writer-notes)

;;; writer-notes.el ends here
