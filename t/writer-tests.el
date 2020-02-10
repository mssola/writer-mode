;;; writer-tests.el --- Tests for writer-mode -*- lexical-binding: t; -*-
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
;; Main file for tests.
;; See CONTRIBUTING.org for documentation on how to run these tests.

;;; Code:

(require 'dash)
(require 'ert)

(require 'writer)
(require 'writer-org)
(require 'writer-tests-utils)


;;; Enabling with an org property


(ert-deftest writer-tests-org-enabled ()
  "When visitting a file with a property set, enable writer-mode automatically."

  (writer-tests-utils-visit-fixture
    "test.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode)))

(ert-deftest writer-tests-org-disabled ()
  "Don't enable writer-mode if the org property was not set for it."

  (writer-tests-utils-visit-fixture
    "no-writer.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (should (equal major-mode 'org-mode))
  (should (null (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))))


;;; Exporting main file


(ert-deftest writer-tests-org-export-to-pdf ()
  "When visiting an ORG file and calling `writer-org-export-to-pdf', the PDF is produced and all the
clutter has been removed."

  ;; Make sure that the out directory does not exist.
  (delete-directory (concat default-directory "t/fixtures/out") t)

  (writer-tests-utils-visit-fixture
    "test.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (writer-org-export-to-pdf)

  ;; No crap, we only have the PDF.
  (should (null (directory-files default-directory t ".tex$")))
  (should (file-exists-p (concat default-directory "out/test.pdf")))

  ;; Clean up
  (delete-directory (concat default-directory "out") t))

(ert-deftest writer-tests-org-export-to-odt ()
  "When visiting an ORG file and calling `writer-org-export-to-odt', the PDF is produced and all the
clutter has been removed."

  ;; Make sure that the out directory does not exist.
  (delete-directory (concat default-directory "t/fixtures/out") t)

  (writer-tests-utils-visit-fixture
    "test.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (writer-org-export-to-odt)

  ;; No crap, we only have the ODT.
  (should (file-exists-p (concat default-directory "out/test.odt")))

  ;; Clean up
  (delete-directory (concat default-directory "out") t))

(ert-deftest writer-tests-org-export-main-file-to-pdf ()
  "When visiting an ORG file and calling `writer-org-export-to-pdf', the PDF
produced is based on the main file."

  ;; Make sure that the out directory does not exist.
  (delete-directory (concat default-directory "t/fixtures/project/out") t)

  (writer-tests-utils-visit-fixture
    "project/test.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (writer-org-export-to-pdf)

  ;; No crap, we only have the PDF.
  (should (null (directory-files default-directory t ".tex$")))
  (should (file-exists-p (concat default-directory "out/main.pdf")))

  ;; Clean up
  (delete-directory (concat default-directory "out") t))


;;; writeroom-mode


(ert-deftest writer-tests-writeroom-mode ()
  "Enable and disable writeroom-mode."

  (writer-tests-utils-visit-fixture
    "test.org"
    (setq writer-forced t)
    (setq writer-notes-hidden t))

  (writer-room)
  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))
  (should (-contains? (writer-tests-utils-which-active-modes) 'writeroom-mode))
  (writer-room-quit)
  (should (-contains? (writer-tests-utils-which-active-modes) 'writer-mode))
  (should (null (-contains? (writer-tests-utils-which-active-modes) 'writeroom-mode))))


(provide 'writer-tests)

;;; writer-tests.el ends here
