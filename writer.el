;;; writer.el --- A minor mode for writers.

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
;; See README.org for documentation on how to use this minor mode.

;;; Code:

(require 'org)
(require 'windmove)

;; Customizable variables.

(defcustom writer-jump-to-first-headline t
  "Whether to jump to the first headline or to the closest one.
When you have multiple levels of heading and you press to jump inside of the
main window, sometimes you want to jump just to the main headline, or to the
closest one (even if it's a deeper level)."
  :group 'writer
  :type '(choice (const :tag "To the first headline" t)
                 (const :tag "To the closest headline" nil)))

(defcustom writer-forced nil
  "Force the execution of this mode.
If this is set to nil, then it will ask permission to the user to
start normally (i.e. delete the other windows).  This option is
given since users might be unaware that this mode is disruptive."
  :group 'writer
  :type '(choice (const :tag "Force execution" t)
                 (const :tag "Ask first" nil)))

;; Mode definition.

;;;###autoload
(define-minor-mode writer-mode
  "Toggle writer mode"

  :init-value nil
  :lighter " Writer"
  :global nil

  (if writer-mode
      (writer--enable)
    (writer--disable)))

;; Functions for jumping between windows.

; Dan from https://emacs.stackexchange.com/a/9532
(defun writer-jump-to-outline ()
  "Switch to a cloned buffer's base buffer.
This function will also move the point to the cursor position in
the clone.  It has largely been taken from
https://emacs.stackexchange.com/a/9532, which has been written by Dan.
I've added the integration with `writer-jump-to-first-headline'."
  (interactive)

  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))

      ; Select the right window and switch to the proper buffer.
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))

      ; Go to the position as given by the cloned buffer.
      (goto-char pos)

      ; If we are to jump to a first level headline, hide everything from the
      ; base and go to the beginning of the line.
      (when writer-jump-to-first-headline
        (progn
          (outline-hide-sublevels 1)
          (org-beginning-of-line)))

      ; Finally, if the given point is not visible, show the needed context.
      (when (invisible-p (point))
        (outline-show-branches)))))

(defun writer-jump-from-outline ()
  "Open a window with a newly cloned buffer from the selected element of the outline."
  (interactive)

  (delete-other-windows)
  (org-tree-to-indirect-buffer)
  (setq fit-window-to-buffer-horizontally 'only)
  (fit-window-to-buffer)
  (windmove-right))

(defun writer-jump ()
  "Call either `writer-jump-from-outline' or `writer-jump-to-outline'."
  (interactive)

  (let ((buf (buffer-base-buffer)))
    (if buf
        (writer-jump-to-outline)
      (writer-jump-from-outline))))

;; Enable and disable the mode, and related functions.

(defun writer-go-to-first-heading ()
  "Move the cursor to the next heading."

  (goto-char (point-min))
  (outline-next-visible-heading 1))

(defun writer-create-workspace ()
  "Create the workspace for this minor mode."

  (outline-hide-sublevels 1)
  (writer-go-to-first-heading)
  (writer-jump-from-outline))

(defun writer-error-and-disable (msg)
  "Write the given error and disable this mode.
`MSG' is the message to be written in the Messages buffer."

  (writer-mode -1)
  (error msg))

(defun writer--enable ()
  "Set up writer mode for the current frame."

  (unless (derived-mode-p 'org-mode)
    (writer-error-and-disable "This minor mode only works in conjuction with Org mode"))

  (when (> (length (window-list)) 1)
    (unless writer-forced
        (unless (yes-or-no-p "This will delete the other windows. Are you sure? ")
          (writer-error-and-disable "User refused to start this mode"))))

  (writer-create-workspace))

(defun writer-delete-other-windows-and-clones ()
  "Delete all the other windows and cloned buffers.
Note that this function assumes that we are on a window which contains the base buffer."

  ; Iterate over the rest of the buffer list (we assume that the first one is
  ; the one that we want to preserve), and remove all buffers that start with
  ; the name of our current buffer.
  (let ((buffers (cdr (buffer-list)))
        (prefix (format "%s-" (buffer-name))))
    (while buffers
      (let ((cur (car buffers)))
        (when (with-current-buffer cur (string-prefix-p prefix (buffer-name cur)))
          (kill-buffer cur)))
      (setq buffers (cdr buffers))))

  ; All the cloned buffers have been killed, now just delete the other windows.
  (delete-other-windows))

(defun writer-only-window-from-buffer (buf)
  "Go to the first window containing `BUF'and delete all the other windows.
If such a window doesn't exist, then the current one will be selected and the
given buffer will be placed on it."

  (let ((w (get-buffer-window buf (selected-frame))))
    (if w
        (select-window w)
      (switch-to-buffer buf)))
  (writer-delete-other-windows-and-clones))

(defun writer--disable ()
  "Remove writer mode for the current frame."

  (let ((buf (buffer-base-buffer)))
    (if buf
        (writer-only-window-from-buffer buf)
      (writer-delete-other-windows-and-clones)))

  ; And finally disable for sure.
  (if writer-mode
      (writer-mode -1)))

(provide 'writer)

;;; writer.el ends here
