;;; build.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Shom Bandopadhaya
;; Copyright (C) 2024 Christina O'Donnell
;;
;; Author: Christina O'Donnell <cdo@mutix.org>
;; Maintainer: Christina O'Donnell <cdo@mutix.org>
;; Created: October 04, 2021
;; Modified: July 09, 2024
;; Version: 0.0.1
;; Keywords: emacs org hugo
;; Homepage: https://git.sr.ht
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: automated build for ox-hugo org mode website
;;
;;; Code:

;; Setup
(message "\n==== Setup package repos ====")
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(message "\n==== Installing depedencies ====")
(package-install 'ox-hugo)
(require 'org-id)
(require 'ox-hugo)

(setq org-hugo-base-dir default-directory)
(setq org-hugo-default-section-directory "")

(setq org-hugo-default-section-directory "blog")

(message "\n==== Exporting Hugo markdown ====")

(let ((files (directory-files-recursively "./org/blog" "\\.org$")))
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (org-hugo-export-wim-to-md :all-subtrees nil :visible-only nil)
      (org-babel-tangle))))

(message "\n==== Export complete ====")

;;; build.el ends here
