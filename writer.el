;;; writer.el --- Main file for this mode -*- lexical-binding: t; -*-
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
;; Main file of this mode.
;; See README.org for documentation on how to use this minor mode.

;;; Code:

(require 'org)
(require 'windmove)

(require 'writer-notes)
(require 'writer-org)

;;; Global & customizable variables.

(defvar writer--room-available nil
  "Whether writeroom is available.")

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

(defcustom writer-pre-hook nil
  "Hook run just *before* setting up the environment.
This hook is called without passing any argument.  Use this hook
to add some special values before doing anything at all (e.g. a
different `line-spacing' than the one given here)"
  :type 'hook
  :group 'writer)

;;; Mode definition.

;;;###autoload
(define-minor-mode writer-mode
  "Toggle writer mode"

  :init-value nil
  :lighter " Writer"
  :global nil

  (if writer-mode
      (writer--enable)
    (writer--disable)))

;;; Functions for jumping between windows.

(defun writer-jump-to-outline ()
  "Switch to a cloned buffer's base buffer.
This function will also move the point to the cursor position in
the clone.  It has largely been taken from
https://emacs.stackexchange.com/a/9532, which has been written by Dan.
I've added the integration with `writer-jump-to-first-headline'."
  (interactive)

  ;; Quit 'writeroom-mode' if we are in it.
  (when (bound-and-true-p writeroom-mode)
    (writer-room-quit))

  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))

      ;; Select the right window and switch to the proper buffer.
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))

      ;; Go to the position as given by the cloned buffer.
      (goto-char pos)

      ;; If we are to jump to a first level headline, hide everything from the
      ;; base and go to the beginning of the line.
      (when writer-jump-to-first-headline
        (progn
          (outline-hide-sublevels 1)
          (org-beginning-of-line)))

      ;; Finally, if the given point is not visible, show the needed context.
      (when (invisible-p (point))
        (outline-show-branches)))))

(defun writer-jump-from-outline ()
  "Open a window with a newly cloned buffer from the selected element of the outline."
  (interactive)

  (delete-other-windows)
  (split-window-right)
  (org-tree-to-indirect-buffer)
  (outline-hide-sublevels 1)

  (dotimes (_ 2)
    (windmove-right)
    (split-window-right))

  (windmove-right)
  (writer-notes-init-and-balance))

(defun writer-jump ()
  "Call either `writer-jump-from-outline' or `writer-jump-to-outline'."
  (interactive)

  (let ((buf (buffer-base-buffer)))
    (if buf
        (writer-jump-to-outline)
      (writer-jump-from-outline))))

;;; writeroom integration

(defun writer-room ()
  "Check some assumptions and enter 'writeroom-mode'."
  (interactive)

  (unless writer--room-available
    (error "You have to install writeroom-mode first"))
  (when (fboundp 'writeroom-mode)
    (writeroom-mode 1)))

(defun writer--room-force-quit (msg)
  "Quit 'writeroom-mode' and leave an error message as provided by `MSG'."
  (when (fboundp 'writeroom-mode)
    (writeroom-mode -1))
  (writer-error-and-disable msg))

(defun writer-room-quit ()
  "Gracefully quit 'writeroom-mode' and leave things as they were before entering."
  (interactive)

  (let ((buf (buffer-base-buffer))
        (cur (current-buffer)))

    ;; First of all, quit 'writeroom-mode'.
    (when (fboundp 'writeroom-mode)
      (writeroom-mode -1))

    ;; There are two cases here:
    ;;  1. If we were writing in a cloned buffer, then we have to switch to the
    ;;     base buffer (which is acting as the outline, so we have to hide the
    ;;     sublevels). Afterwards we have to remake the environment and finally
    ;;     switch to the cloned buffer in the middle.
    ;;  2. If this was not a cloned buffer, then we can simply restart the
    ;;     environment from scratch.
    (if buf
        (progn
          (switch-to-buffer buf)
          (outline-hide-sublevels 1)

          (dotimes (_ 3)
            (split-window-right)
            (windmove-right))

          (writer-notes-init-and-balance)
          (switch-to-buffer cur))
      (writer-notes-create-workspace))))

;; Enable and disable the mode, and related functions.

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

  (if (require 'writeroom-mode nil t)
      (setq writer--room-available t)
    (message "writeroom-mode is not installed, disabling its integration"))
  (unless (require 'olivetti nil t)
    (message "olivetti is not installed, disabling its integration"))

  (writer-setup))

(defun writer-setup ()
  "Setup the environment.
This consists of two windows, were the one on the right takes a 1/4 of the total
available space.  It also sets values to variables such as `line-spacing' and it
starts olivetti mode with 100 columns."

  (setq line-spacing 3)
  (flyspell-mode 1)
  (setq global-hl-line-mode nil)
  ;; `set-fringe-mode' was introduced in GNU Emacs 25.2 and we should support GNU Emacs 25.1 too.
  (when (fboundp 'set-fringe-mode)
    (set-fringe-mode 0))

  (run-hook-with-args writer-pre-hook)
  (writer-notes-create-workspace)
  (when (fboundp 'olivetti-mode)
    (olivetti-mode 1))
  (when (fboundp 'olivetti-set-width)
    (olivetti-set-width 100)))


(defun writer-delete-other-windows-and-clones ()
  "Delete all the other windows and cloned buffers.
Note that this function assumes that we are on a window which contains the base buffer."

  ;; Iterate over the rest of the buffer list (we assume that the first one is
  ;; the one that we want to preserve), and remove all buffers that start with
  ;; the name of our current buffer.
  (let ((buffers (cdr (buffer-list)))
        (prefix (format "%s-" (buffer-name))))
    (while buffers
      (let ((cur (car buffers)))
        (when (with-current-buffer cur (string-prefix-p prefix (buffer-name cur)))
          (kill-buffer cur)))
      (setq buffers (cdr buffers))))

  ;; All the cloned buffers have been killed, now just delete the other windows.
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

  ;; Disable for sure.
  (if writer-mode
      (writer-mode -1))

  ;; And finally kill this buffer. This is safe because all variables are local.
  (kill-this-buffer))

(defun writer--enable-maybe ()
  "Enable this mode depending on `writer-org-enable-p'.
That is, this function will enable this mode if the user set 'writer-mode:t' as
an 'org-mode' property."

  (when (and (writer-org-enable-p) (not writer-mode))
    (writer-mode)))

;;; Hooks

;; Automatically enable this mode if there is the org-mode property writer-mode is set to 't'.
(add-hook 'org-mode-hook 'writer--enable-maybe)

(provide 'writer)

;;; writer.el ends here
