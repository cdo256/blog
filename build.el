;;; build.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Shom Bandopadhaya
;; Copyright (C) 2024 Christina O'Donnell
;;
;; Author: Shom Bandopadhaya <https://github.com/shombando>
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
;;  Description
;;
;;; Code:

;; Setup
(message "\n==== Setup package repos ====")
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install and dependencies
(message "\n==== Installing depedencies ====")
(package-install 'ox-hugo)
(require 'org-id)
(require 'ox-hugo)

;; Export content from org to Hugo md
(message "\n==== Exporting Hugo markdown ====")
(with-current-buffer (find-file "./org/main.org")
  (org-hugo-export-wim-to-md :all-subtrees nil :visible-only nil))

(with-current-buffer (find-file "./org/posts.org")
  (org-hugo-export-wim-to-md :all-subtrees nil :visible-only nil))
(org-babel-tangle-file "./org/posts.org")

;; (with-current-buffer (find-file "./start.org")
;;   (org-hugo-export-wim-to-md :all-subtrees nil :visible-only nil))


(message "\n==== Export complete ====")

;;; build.el ends here
