;;; writer-interactive-tests.el --- Interactive tests for writer-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 Miquel Sabaté Solà <mikisabate@gmail.com>
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
;; Main file for interactive tests.
;; See CONTRIBUTING.org for documentation on how to run these tests.

;;; Code:

(require 'dash)
(require 'ert)
(require 'org)

(require 'writer)
(require 'writer-tests-utils)

;;; The main function to be used when running interactive tests.


(defun writer-interactive-tests-run ()
  "Run all the interactive test suite.
This wraps ERT so GNU Emacs exits with a proper exit code."

  (ert t)

  ;; Save the output.
  (let ((path (expand-file-name (concat default-directory "t/tmp/interactive-results.txt"))))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) path nil nil nil 'confirm)))

  ;; Check if there was a failing test and kill Emacs with the proper exit code.
  (let ((out (buffer-string))
        (re (concat (regexp-quote "Failed") ":.+\\([0-9]\\)"))
        (should-not-exit (string= (getenv "WRITER_NO_EXIT") "t"))
        ret)

    (save-match-data
      (and (string-match re out))
      (setq ret (match-string 1 out)))

    (unless should-not-exit
      (if (string= ret "0")
          (kill-emacs)
        (kill-emacs 1)))))


;;; writer-jump


(ert-deftest writer-interactive-tests-jump ()
  "Jump back and forth from the outline and check the results."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  ;; Simply jump from the first section.

  (goto-char (point-min))
  (org-next-visible-heading 1)
  (writer-jump)
  (should (equal (safe-length (window-list)) 3))

  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))
  (windmove-left)
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)

  ;; Now let's jump from the main window to the outline.

  (writer-jump)
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))

  ;; Finally go to the next visible heading and jump again.

  (org-next-visible-heading 1)
  (writer-jump)
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Subsection-1")))


;;; Toggling the notes section.


(ert-deftest writer-interactive-tests-notes-available ()
  "Open up a document and simply enable the mode.
This should create an environment with two windows open: the main document and the notes.org one."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))

  (should (equal (safe-length (window-list)) 2))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (other-window 1)
  (should (equal (buffer-name (window-buffer)) "notes.org")))

(ert-deftest writer-interactive-tests-notes-hidden-at-startup ()
  "Open up a document and enable the mode in a minimalistic way.
    Before enabling the mode we set `writer-notes-hidden' to true, so the environment won't create a
    window for the notes.org file."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden t)
    (setq writer-forced t))

  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))

  (should (equal (safe-length (window-list)) 1))
  (should (equal (buffer-name (window-buffer)) "no-writer.org")))

(ert-deftest writer-interactive-tests-hide-notes ()
  "Enable the mode and hide the notes section."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  (writer-notes-hide)
  (should (equal (safe-length (window-list)) 1))
  (should (equal (buffer-name (window-buffer)) "no-writer.org")))

(ert-deftest writer-interactive-tests-toggle-notes-one-window ()
  "Toggle the notes section when we have only one window."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  (writer-notes-toggle)
  (should (equal (safe-length (window-list)) 1))
  (writer-notes-toggle)
  (should (equal (safe-length (window-list)) 2))
  (writer-notes-toggle)
  (should (equal (safe-length (window-list)) 1)))

(ert-deftest writer-interactive-tests-toggle-notes-two-windows ()
  "Toggle the notes section when having multiple windows open."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  ;; Force the creation of an outline. This way we have a realistic "full" scenario.
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (writer-jump)
  (should (equal (safe-length (window-list)) 3))

    ;;; Let's toggle the section while being at the left-most window.

  ;; Toggle the notes section from the left-most window, toggle, move to the left-most window again
  ;; and check the name of the buffers.
  (while (window-in-direction 'left)
    (windmove-left))
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 2))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))

  ;; Now let's make it re-appear and perform the same checks.
  (while (window-in-direction 'left)
    (windmove-left))
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "notes.org"))

    ;;; Now let's do the same but the action will be triggered from the window at the center.

  (while (window-in-direction 'left)
    (windmove-left))
  (windmove-right)
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 2))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))

  ;; Now let's make it re-appear and perform the same checks.
  (while (window-in-direction 'left)
    (windmove-left))
  (windmove-right)
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "notes.org"))

    ;;; And finally let's trigger the action from the notes.org window.

  (while (window-in-direction 'right)
    (windmove-right))
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 2))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))

  ;; Now let's make it re-appear and perform the same checks.
  (while (window-in-direction 'right)
    (windmove-right))
  (writer-notes-toggle)
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "notes.org")))


    ;;; Sizes


(ert-deftest writer-interactive-tests-proper-sizes ()
  "Check that the windows have the right sizes in any situation."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  ;; The window on the left (main window) should be more or less three times bigger than the notes
  ;; section.
  (let ((bigger (window-total-width))
        (smaller (window-total-width (other-window 1))))
    (should (writer-tests-utils-more-or-less-p smaller bigger 3)))

  (windmove-left)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (writer-jump)
  (while (window-in-direction 'left)
    (windmove-left))

  ;; When there are three windows, then the one at the center should be, more or less, twice as big
  ;; as the other ones.
  (let ((left (window-total-width))
        (center (window-total-width (other-window 1)))
        (right (window-total-width (other-window 2))))

    (should (writer-tests-utils-more-or-less-p left center 2))
    (should (writer-tests-utils-more-or-less-p right center 2))))


    ;;; writeroom-mode


(ert-deftest writer-interactive-tests-writeroom-only-one-window ()
  "Check that writeroom-mode is enabled properly."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  (writer-room)
  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))
  (should (-contains? (writer-tests-utils-which-active-modes) 'writeroom-mode))
  (should (equal (safe-length (window-list)) 1)))

(ert-deftest writer-interactive-tests-writeroom-multiple-windows ()
  "Check that the environment is preserved when going back from writeroom-mode."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-notes-hidden nil)
    (setq writer-forced t))

  ;; Setting up an environment with three windows.

  (goto-char (point-min))
  (org-next-visible-heading 1)
  (writer-jump)

  ;; When entering writeroom-mode, the proper buffer should be picked up.

  (writer-room)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))

  ;; When quitting writeroom-mode, the previous environment must be preserved.

  (writer-room-quit)
  (should (equal (safe-length (window-list)) 3))
  (while (window-in-direction 'left)
    (windmove-left))
  (should (equal (safe-length (window-list)) 3))
  (should (equal (buffer-name (window-buffer)) "no-writer.org"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "no-writer.org-Section-1"))
  (windmove-right)
  (should (equal (buffer-name (window-buffer)) "notes.org")))

(provide 'writer-interactive-tests)

;;; writer-interactive-tests.el ends here
