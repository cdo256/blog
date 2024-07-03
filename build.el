;; Adapted from https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/

;; (require 'package)
;; (setq package-user-dir (expand-file-name "./.packages"))
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("nongnu" . "https://elpa.nongnu.org/packages/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (defun file-to-string (file)
;;   "Read the content of FILE and return it as a string."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (buffer-string)))

(require 'htmlize)
(require 'ox-publish)
(require 'ox-hugo)

(setq org-publish-project-alist
      `(("octocurious:main"
         :recursive t
         :base-directory "./org"
         :publishing-directory "./content"
         :publishing-function org-hugo-export-as-md
         :with-creator t
         :with-toc f
         :section-numbers nil
         :email "christina@octocurious.com"
         :language "English")))

(org-publish-all t)

(message "Build complete!")
