;; Adapted from https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(package-install 'htmlize)
(require 'ox-publish)
(use-package 'ox-hugo
  :ensure t)

(defun html-publish-to-html (plist filename pub-dir)
  "Publish an HTML file with a preamble and postamble."
  (let* ((html (with-temp-buffer
                 (insert-file-contents filename)
                 (buffer-string)))
         (output (html-publish-template)))
    (with-temp-buffer
      (insert output)
      (write-file (concat pub-dir (file-name-nondirectory filename))))))

(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil)

(setq org-publish-project-alist
      `(("octocurious:main"
         :recursive t
         :base-directory "./org"
         :publishing-directory "./content"
         :publishing-function org-hugo-publish-as-md
         :with-creator t
         :with-toc f
         :section-numbers nil
         :email "christina@octocurious.com"
         :language "English")))

(org-publish-all t)

(message "Build complete!")
