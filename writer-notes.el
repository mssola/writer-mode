;;; writer-notes.el --- Support for the notes section  -*- lexical-binding: t; -*-
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
;; Code for handling the notes section.
;;
;;; Code:

(require 'windmove)

(defcustom writer-notes-post-hook nil
  "Hook run just *after* setting up the notes section.
This hook is called without passing any argument.  Use this hook
to further tune the look and feel of the right section (e.g. opening up a new
window below with dired mode in it)."
  :type 'hook
  :group 'writer)

(defcustom writer-notes-hidden nil
  "Whether to display or not the notes section.
If this is set to t, then it will not open up the notes section when creating
the environment.  This variable is also used internally for functions that
hide/display this section.  This can always be reversed on runtime by calling
the 'writer-notes-show' function."
  :group 'writer
  :type '(choice (const :tag "Hide notes section" t)
                 (const :tag "Create the notes section" nil)))

(defconst writer-notes-file "notes.org"
  "The name of the file holding the notes.")

;;; Creation and initialization

(defun writer-notes-create-workspace (&optional mid-creation)
  "Create the environment from scratch.
For that, it assumes that there is only one window on the frame, and it then
allocates a 1/4 on the right for the notes section.  Otherwise, if MID-CREATION
was set to true, then this function assumes that two windows are already present
and that the cursor is currently at the rightmost window."

  (let ((dec (if mid-creation 1 0)))
    (unless writer-notes-hidden
      (dotimes (_ (- 3 dec))
        (split-window-right)
        (windmove-right))

      (writer-notes-init)
      (run-hook-with-args writer-notes-post-hook)
      (balance-windows)

      (dotimes (_ (- 2 dec))
        (windmove-left)
        (delete-window))
      (windmove-left))))

(defun writer-notes-init ()
  "Allocate the contents of the notes section."

  (find-file writer-notes-file t))

(defun writer-notes-init-and-balance ()
  "Initialize the notes space and balance the environment.
As for the environment, it assumes that there are 4 opened
vertical windows, the rightmost of which corresponds to the
current one."

  (unless writer-notes-hidden
    (writer-notes-init))
  (balance-windows)
  (windmove-left)
  (delete-window)
  (if writer-notes-hidden
      (delete-window)
    (windmove-left)))

;;; Show/hide section

(defun writer-notes-show ()
  "Show the notes section."

  (interactive)

  (if writer-notes-hidden
      (writer-notes--show)
    ;; If it's supposed to be shown but its window could not be located, then simply recreate the
    ;; environment by showing the notes section again. Otherwise, if there is a window containing
    ;; this buffer, just display an error message.
    (let ((cw (writer-notes--file-window)))
      (if cw
          (error "Notes section is already visible in window %s" cw)
        (writer-notes--show)))))

(defun writer-notes-hide ()
  "Hide the notes section if possible.
Do nothing if there is only one window available, and write an
error if we are supposed to have this section already hidden.
If none of these cases apply, it will hide the notes section."

  (interactive)

  (if writer-notes-hidden
      (error "Notes section should already be hidden")
    (progn
      ;; If there is only one window there are two options: either the current window is the notes
      ;; file, or it's another one. In the first case, then we simply need to kill the current
      ;; buffer. Otherwise we have to do nothing.
      (if (= (count-windows) 1)
          (let ((cw (writer-notes--file-window)))
            (setq writer-notes-hidden t)
            (if cw
                (kill-this-buffer)
              (error "There's only one window, doing nothing")))
        (writer-notes--hide)))))

(defun writer-notes-toggle ()
  "Toggle between showing/hiding the notes section."

  (interactive)

  (if writer-notes-hidden
      (writer-notes-show)
    (writer-notes-hide)))

(defun writer-notes--show ()
  "Recreate the notes section given the amount of windows visible.
This will error out if the amount of windows is more than 2, because this is a
scenario which is not supported right now by this mode."

  (setq writer-notes-hidden nil)
  (cond
   ((= (count-windows) 1) (writer-notes-create-workspace))
   ((= (count-windows) 2) (writer-notes--show-two))
   (t (progn
        (setq writer-notes-hidden t)
        (error "This is not possible with so many windows open")))))

(defun writer-notes--show-two ()
  "Recreate the notes section by assuming that there are two windows."

  (while (window-in-direction 'right)
    (windmove-right))
  (writer-notes-create-workspace t))

(defun writer-notes--file-window ()
  "Fetch the window containing the buffer with the notes file."

  (let (ret buf (name (expand-file-name writer-notes-file)))
    (dolist (window (window-list))
      (setq buf (window-buffer window))
      (if (string= (buffer-file-name buf) name)
          (setq ret window)))
    ret))

(defun writer-notes--kill-current-window (name)
  "Delete the current window and kill its buffer if it's the notes file.
Since this function is part of a loop and we don't want to compute the complete
file of the path over and over again, this function expects the caller to pass
the full path as the NAME."

  ;; Kill the buffer containing the notes file. Delete the window always.
  (when (string= name (buffer-file-name (current-buffer)))
    (kill-this-buffer))
  (delete-window)

  ;; It may happen that after deleting the window the cursor jumps to the left sibling. Move back to
  ;; the right.
  (if (window-in-direction 'right)
      (windmove-right)))

(defun writer-notes--kill-all-from-current-window ()
  "Delete all the vertical windows from the point of view of the current one.
This function will also kill the buffer containing the notes file."

  ;; This is implemented by moving up above, and then iterating while going down. On each iteration
  ;; it will call `writer-notes--kill-current-window'. Finally, it will move to the rightmost window
  ;; since this is the expected location of the main file.

  (while (window-in-direction 'above)
    (windmove-up))

  (let ((name (expand-file-name writer-notes-file)))
    (while (window-in-direction 'below)
      (writer-notes--kill-current-window name))
    ;; The iteration stopped when only one window was left, so we have to kill this one too.
    (writer-notes--kill-current-window name))

  (while (window-in-direction 'right)
    (windmove-right)))

(defun writer-notes--hide ()
  "Hide the notes section.
This is done by first locating the window containing the notes file.  It will
error out if this window could not be located."

  (let ((cw (writer-notes--file-window)))
    (if cw
        (progn
          (select-window cw t)
          (writer-notes--kill-all-from-current-window)
          (setq writer-notes-hidden t))
      (error "Could not locate window containing the file: %s" writer-notes-file))))

(provide 'writer-notes)

;;; writer-notes.el ends here
