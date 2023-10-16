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
         :base-directory "./src/octocurious/pages"
         :publishing-directory "./out/octocurious"
         :publishing-function org-html-publish-to-html
         :with-creator t
         :with-toc f
         :section-numbers nil
         :email "christina@octocurious.com"
         :language "English"
         :html-head ,(file-to-string "./out/octocurious/head.html")
         :html-preamble ,(file-to-string "./out/octocurious/header.html")
         :html-postamble ,(file-to-string "./out/octocurious/footer.html")
         :html-divs ((preamble "header" "header")
                     (content "main" "content")
                     (postamble "footer" "footer")))

        ("octocurious:common"
         :recursive t
         :base-directory "./org/common"
         :publishing-directory "./out/octocurious"
         :publishing-function org-html-publish-to-html
         :with-creator t
         :with-toc f
         :section-numbers nil
         :email "christina@octocurious.com"
         :language "English"
         :html-preamble ,(file-to-string "./static/octocurious-header.html")
         :html-postamble ,(file-to-string "./static/octocurious-footer.html")
         :html-divs ((preamble "header" "header")
                     (content "main" "content")
                     (postamble "footer" "footer")))

        ("octocurious:static"
         :recursive t
         :base-directory "./static"
         :base-extension ".*"
         :publishing-directory "./out/octocurious/static"
         :publishing-function org-publish-attachment)

        ("mutix:main"
         :recursive t
         :base-directory "./org/mutix"
         :publishing-directory "./out/mutix"
         :publishing-function org-html-publish-to-html
         :with-creator t
         :with-toc t
         :section-numbers nil
         :email "cdo@mutix.org"
         :language "English"
         :html-preamble ,(file-to-string "./static/mutix-header.html")
         :html-postamble ,(file-to-string "./static/mutix-footer.html")
         :html-divs ((preamble "header" "header")
                      (content "main" "content")
                      (postamble "footer" "footer")))

        ("mutix:common"
         :recursive t
         :base-directory "./org/common"
         :publishing-directory "./out/mutix"
         :publishing-function org-html-publish-to-html
         :with-creator t
         :with-toc t
         :section-numbers nil
         :email "cdo@mutix.org"
         :language "English"
         :html-preamble ,(file-to-string "./static/mutix-header.html")
         :html-postamble ,(file-to-string "./static/mutix-footer.html")
         :html-divs ((preamble "header" "header")
                     (content "main" "content")
                     (postamble "footer" "footer")))

        ("mutix:static"
         :recursive t
         :base-directory "./static"
         :base-extension ".*"
         :publishing-directory "./out/mutix/static"
         :publishing-function org-publish-attachment)))

(org-publish-all t)

(message "Build complete!")
