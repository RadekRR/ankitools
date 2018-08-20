;;; ox-anki.el --- Export org-mode to Anki

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install ox-anki
;;
;; In your .emacs
;;
;;      (require 'ox-anki)
;;
;; For more information, please see `README.org'
;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ox-html)
;;(require 'f)

(defvar org-anki-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Get the absolute path of this file. Don't change this manually.")

(defgroup org-export-anki nil
  "Options for exporting Org mode files to HTML5 slide."
  :tag "Org Export to Google I/O HTML5 slide"
  :group 'org-export)


;;; User Configuration Variables

(defcustom org-anki-config-file
  "slide_config.js"
  "Slide config file for Google I/O slide."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-hlevel 1
  "The minimum level of headings that should be grouped into
vertical slides."
  :group 'org-export-anki
  :type 'integer)

(defvar org-anki--current-footnote-list nil)
(defvar org-anki--footnote-count-in-slide 0)

;;; Define Back-End

(org-export-define-derived-backend 'anki 'html
  
  :menu-entry
  '(?i "Export to Aanki"
       ((?H "As CSV buffer" org-anki-export-as-csv)
        (?h "As CSV file" org-anki-export-to-csv))
       (?o "As CSV file and open"
           (lambda (a s v b)
             (if a (org-anki-export-to-csv t s v b)
               (org-open-file (org-anki-export-to-csv nil s v b))))))

  ;;:filters-alist '((:filter-parse-tree . org-anki-generate-config-file))

  :options-alist
  '(
    ;; Overwrite the HTML_HEAD defined in ox-html.el
    ;;(:html-head "HTML_HEAD" nil "" newline)

    ;; Configs that will be generated in slide_config.js

    ;; subtitle
    ;; (:subtitle          "SUBTITLE"          nil   nil   t)
    ;; ;; useBuilds, Default: true, False will turn off slide animation builds.
    ;; (:use-builds        "USE_BUILDS"        nil "true"  t)
    ;; ;; usePrettify, Default: true
    ;; (:use-prettify      "USE_PRETTIFY"      nil "true"  t)
    ;; ;; enableSlideAreas, Default: true. False turns off the click
    ;; ;; areas on either slide of the slides.
    ;; (:enable-slideareas "ENABLE_SLIDEAREAS" nil "true"  t)
    ;; ;; enableTouch, Default: true. If touch support should enabled.
    ;; ;; Note: the device must support touch.
    ;; (:enable-touch      "ENABLE_TOUCH"      nil "true"  t)
    ;; ;; favIcon
    ;; (:fav-icon          "FAVICON"           nil "images/emacs-icon.png" t)
    ;; (:hash-tag          "HASHTAG"           nil "" t)
    ;; ;; TODO: fonts
    ;; ;; Author information
    ;; ;; (:company           "COMPANY"           nil   nil   t)
    ;; ;; (:google-plus       "GOOGLE_PLUS"       nil   nil   t)
    ;; ;; (:twitter           "TWITTER"           nil   nil   t)
    ;; ;; (:www               "WWW"               nil   nil   t)
    ;; ;; (:github            "GITHUB"            nil   nil   t)


    ;; ;; Enable user add #+THEME: io2013 to select theme from theme/css/io2013.css
    ;; ;; FIXME: currently the io2013.css is not work, we use theme/css/default.css by default
    ;; (:theme             "THEME"             nil   nil   t)

    ;; Other configs

    ;; Use MathJax, Default: true. False will remove MathJax from
    ;; current slide to save space (when exporting); True will
    ;; re-install it again.
    ;;(:use-mathjax       "USE_MATHJAX"       nil "true"  t)
    ;; Google analytics: 'UA-XXXXXXXX-1
    ;;(:analytics         "ANALYTICS"         nil   nil   t)
    ;;(:logo              "LOGO"              nil    ""   t)
    ;;(:icon              "ICON"              nil "images/emacs-icon.png" t)
    ;;(:hlevel            "HLEVEL"            nil   nil   t)

    ;; TODO: idea ?
    ;; Hide the default title slide
    ;; (:hide-title-slide  "HIDE_TITLE_SLIDE" nil    nil   t)
    )
  :translate-alist
  '(
    (paragraph . org-anki-paragraph)
    (headline . org-anki-headline)
    ;; (plain-text . org-anki-plain-text)
    (section . org-anki-section)
    (inner-template . org-anki-inner-template)
    (template . org-anki-template)
    (underline . org-anki-underline)
    )
  ;; :translate-alist
  ;; '(
  ;;   ;; (headline                   .       org-anki-headline)
  ;;   ;; (section                    .       org-anki-section)
  ;;   ;; (template                   .       org-anki-template)
  ;;   ;; (center-block               .       org-anki-center-block)
  ;;   ;; (src-block                  .       org-anki-src-block)
  ;;   ;; (quote-block                .       org-anki-quote-block)
  ;;   ;; (verse-block                .       org-anki-verse-block)
  ;;   ;; (table-cell                 .       org-anki-table-cell)
  ;;   ;; (export-block               .       org-anki-export-block)
  ;;   ;; (plain-list                 .       org-anki-plain-list)
  ;;   ;; (paragraph                  .       org-anki-paragraph)
  ;;   ;; (inner-template             .       org-anki-inner-template)
  ;;   ;; (footnote-definition        .       org-anki-footnote-definition)
  ;;   ;; (footnote-reference         .       org-anki-footnote-reference)
  ;;   )

  )

(defun org-anki-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."

  text)

(defun org-anki-underline (_underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<u>%s</u>" contents))

;;; Internal Functions
(defun org-anki--plist-get-string (info key)
  (let ((r (plist-get info key)))
    (if (stringp r) r (or (car r) ""))))

(defun org-anki-close-element (element attr body)
  (format "<%s %s>\n%s\n</%s>" element attr body element))

(defun org-anki-close-element* (element attr body)
  "What is this?!"
  (format "</%s>\n%s\n<%s %s>\n" element body element attr))

(defun org-anki--copy-resource ()
  "Copy needed resource to current path."
  ;; TODO: make user use their own template instead of force copy
  ;; FIXME: do not copy image folder ?
  (mapc (lambda (dir)
          (copy-directory (f-join org-anki-path dir) dir))
        '("js/" "images/" "theme/")))

(defun org-anki-check-resource ()
  "Check js/slides.js exist or not, if not exist, re-fetch resource."
  (if (not (file-exists-p "js/slides.js"))
      (org-anki--copy-resource)))

(defun org-anki-generate-small-icon-css (icon-path hash-tag)
  "Generate theme/css/small-icon.css to overwrite style.
(The small icon at the left bottom corner)"
  (progn
    (save-excursion
      (with-temp-file "theme/css/small-icon.css"
        (insert "slides > slide:not(.nobackground):before {
background: url(../../" icon-path ") no-repeat 0 50%;
font-size: 12pt;
content: \"" hash-tag "\";
position: absolute;
bottom: 20px;
left: 60px;
-moz-background-size: 30px 30px;
-o-background-size: 30px 30px;
-webkit-background-size: 30px 30px;
background-size: 30px 30px;
padding-left: 40px;
height: 30px;
line-height: 1.9;
}")))
    ""))

(defun org-anki-generate-config-file (text back-end info)
  (let ((file-name org-anki-config-file))
    (save-excursion
      (with-temp-file file-name
        (insert
         (concat
          "var SLIDE_CONFIG = {
   // Slide Settings
   settings: {
"
          ;; title
          (format
           "     title: '%s', \n" (org-anki--plist-get-string info :title))
          ;; subtitle
          (format
           "     subtitle: '%s', \n" (org-anki--plist-get-string info :subtitle))
          ;; useBuilds
          (format
           "     useBuilds: %s, " (org-anki--plist-get-string info :use-builds))
          "// Default: true. False will turn off slide animation builds. \n"
          ;; usePrettify
          (format
           "     usePrettify: %s, " (org-anki--plist-get-string info :use-prettify))
          "// Default: true \n"
          ;; enableSlideAreas
          (format
           "     enableSlideAreas: %s, " (org-anki--plist-get-string info :enable-slideareas))
          "// Default: true. False turns off the click areas on either slide of the slides.\n"
          ;; enableTouch
          (format
           "     enableTouch: %s, " (org-anki--plist-get-string info :enable-touch))
          "// Default: true. If touch support should enabled. Note: the device must support touch.\n"
          ;; favIcon
          (format
           "     favIcon: '%s', \n" (org-anki--plist-get-string info :fav-icon))
          ;; TODO: fonts
          "     fonts: [
       'Open Sans:regular,semibold,italic,italicsemibold',
       'Source Code Pro'
     ],\n"
          "   }, \n \n"
          ;; Author information
          "   // Author information
   presenters: [{\n"
          ;; name
          (format
           "     name: '%s', \n" (org-anki--plist-get-string info :author))
          ;; company
          (format
           "     company: '%s', \n" (org-anki--plist-get-string info :company))
          ;; google plus
          (format
           "     gplus: '%s', \n" (org-anki--plist-get-string info :google-plus))
          ;; twitter
          (format
           "     twitter: '%s', \n" (org-anki--plist-get-string info :twitter))
          ;; www
          (format
           "     www: '%s', \n" (org-anki--plist-get-string info :www))
          ;; github
          (format
           "     github: '%s', \n" (org-anki--plist-get-string info :github))

          "   }]
};"
          ))))))


;;; Transcode Functions

(defun org-anki-get-hlevel (info)
  "Get HLevel value safely.
If option \"HTML5SLIDE_HLEVEL\" is set, retrieve integer value from it,
else get value from custom variable `org-anki-hlevel'."
  (let ((hlevel-str (plist-get info :hlevel)))
    (if hlevel-str (string-to-number hlevel-str)
      org-anki-hlevel)))

;;;; Center Block
(defun org-anki-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format
   "<article class=\"flexbox vcenter\">\n%s</article>" contents))


;;;; Note Block
(defun org-anki-export-block (export-block contents info)
  "Transocde a EXPORT-BLOCK element from Org to anki.
CONTENTS is nil. NFO is a plist holding contextual information."
  (if (string= (org-element-property :type export-block) "NOTES")
      (concat
       "<aside class=\"note\">\n <section>\n"
       (org-element-property :value export-block)
       "</section>\n</aside>\n")
    (org-html-export-block export-block contents info)))

;;;; Quote Block
(defun org-anki-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((parent (org-export-get-parent-headline quote-block))
         (slide-prop (org-element-property :SLIDE parent))
         (attributes (org-export-read-attribute :attr_html quote-block))
         (class (plist-get attributes :class))
         (--make-sign (function
                       (lambda (string)
                         (replace-regexp-in-string
                          "^ *\\(&#x201[34];\\)\\(.+\\)\\(<br */>\\|\n\\)"
                          "<span class='float-right'>\\1\\2</span>\\3" string)))))
    (if (and class (string-match "notes?" class))
        (format "<aside class=\"note\">
  <section>
%s
  </section>
</aside>
" contents)
      (if (and slide-prop
               (string-match "segue" slide-prop))
          ;; [FIXME] different sign rendering under Firefox and Chrome...
          (format "<q>\n%s</q>"
                  (replace-regexp-in-string
                   "<br>\n *<span" "<span"
                   (replace-regexp-in-string
                    "</?p>" ""
                    (replace-regexp-in-string
                     "</?p>\n* *<p>" "<br>"
                     (funcall --make-sign contents)))))
        (format "<blockquote>\n%s</blockquote>"
                ;; Align "-- Name" to right side.
                (save-match-data
                  (replace-regexp-in-string
                   "</span>\n</p>"
                   "</span><br  />\n</p>"
                   (funcall --make-sign contents))))))))


;;; Verse Block
(defun org-anki-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Align "-- Name" to right side.
  (replace-regexp-in-string "^ *&#xa0;\\(?:&#xa0;\\)+\\(&#x201[34];\\)\\(.+?\\)\\(<br */>\\|\n\\)"
                            "<span class='float-right'>\\1\\2</span>\\3"
                            (org-html-verse-block verse-block contents info)))

;;;; Paragraph

(defun org-anki-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  ;; Fix multibyte language like chinese will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.

  ;; (if (/= 1 (org-export-get-relative-level (org-export-get-parent-headline paragraph) info))
  ;;     (org-export-data paragraph info)
  ;;   (org-export-data "" info)))

  
  (if (org-export-get-parent-headline paragraph)
      ;; (if (org-export-get-previous-element paragraph info)
      ;; 	  (format "<p>%s</p>" contents)
      ;; 	contents)
      (format "<p>%s</p>" contents)
    ""))
  

  ;; (if (equal 'headline (org-element-property :type (org-export-get-previous-element paragraph info)))
  ;;     (concat "pohead:" contents)
  ;;   (concat "niehead:" contents)))

  ;; (let ((fix-contents
  ;;        (replace-regexp-in-string
  ;;         (concat "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)") "\\1\\2" contents)))

  ;;   ;; Send modify data to org-html-paragraph
  ;;   (org-html-paragraph paragraph fix-contents info)))

;;;; Src Block

(defun org-anki--encode-src-text (code)
  "Encode source code to plane text, if source contains <b> or </b>,
make it translate to html tags."
  (replace-regexp-in-string
   "&lt;b&gt;"  "<b>"
   (replace-regexp-in-string
    "&lt;/b&gt;" "</b>"
    (org-html-encode-plain-text code))))

(defun org-anki--src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code
           ;; If not use-prettify, use org-html's method
           ;; to renderd src block
           (if (string= "true" (plist-get info :use-prettify))
               (org-anki--encode-src-text (car (org-export-unravel-code src-block)))
             (org-html-format-code src-block info)))

          (label (let ((lbl (org-element-property :name src-block)))
                   (if (not lbl) ""
                     (format " id=\"%s\""
                             (org-export-get-reference lbl info))))))
      (if (not lang)
          ;; Use org-html-src-block to genterate HTML code.
          (org-html-src-block src-block contents info)
        ;; Use prettyprint to generate source block
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n<pre class=\"prettyprint\" data-lang=\"%s\"%s>\n%s</pre>\n" lang label code))

        ))))

(defun org-anki-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
If #+USE_PRETTIFY is `true' use org-anki--src-block to render the code block.
Else use org-html-src-block to convert source block to html."
  (org-anki--src-block src-block contents info))

;;;; Google Analytics
(defun org-anki-google-analytics (info)
  (let ((user-id (plist-get info :analytics)))
    (if (null user-id)
        ""
      (format
       (concat "<script>\n"
               "var _gaq = _gaq || []; \n"
               "_gaq.push(['_setAccount', '%s']);\n"
               "_gaq.push(['_trackPageview']); \n"
               "(function() {
  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();
"
               "</script>\n"
               ) user-id))))

;;;; headline

(defun org-anki-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Google I/O slides.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  (let*  ((id (org-element-property :ID headline))
	  (level (org-export-get-relative-level headline info))
	  (headstr (if (= level 1)
		       (concat "<cardseparator/>" id "<fieldseparator/>")
		     "<fieldseparator/>"
		     ))
	  (mediastr ""))
    
    (when (and id (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" id ".mp3")))
      (setq mediastr (format "%s[sound:%s.mp3] " mediastr id)))
    (when (and id (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" id ".jpg")))
      (setq mediastr (format "%s<img src=\"%s.jpg\"/> " mediastr id)))
    
    ;; (when (org-element-property :SUBMP3 headline)
    ;;   (setq mediastr (format "%s[sound:%s] " mediastr (org-element-property :SUBMP3 headline))))
    ;; (when (org-element-property :SUBIMG headline)
    ;;   (setq mediastr (format "%s<img src=\"%s\"/> " mediastr (org-element-property :SUBIMG headline))))

    (setq mediastr (concat mediastr "<fieldseparator/>"))
    (format "%s%s%s%s" headstr mediastr (org-export-data (org-element-property :title headline) info) (if contents
												contents
											      ""))))
    

;;  (org-export-data (org-element-property :title headline) info))
  ;; (if (/= 1 (org-export-get-relative-level headline info))
  ;;   (format "<blek level %d>%s</blek>%s"
  ;; 	    (org-export-get-relative-level headline info)
  ;; 	    (org-export-data (org-element-property :title headline) info)
  ;; 	    contents)
  ;;   ))

  ;; ;; First call org-html-headline to get the formatted HTML contents.
  ;; ;; Then add enclosing <article> tags to mark slides.
  ;; (setq contents (or contents ""))
  ;; (let* ((numberedp (org-export-numbered-headline-p headline info))
  ;;        (level (org-export-get-relative-level headline info))
  ;;        (text (org-export-data (org-element-property :title headline) info))
  ;;        (todo (and (plist-get info :with-todo-keywords)
  ;;                   (let ((todo (org-element-property :todo-keyword headline)))
  ;;                     (and todo (org-export-data todo info)))))
  ;;        (todo-type (and todo (org-element-property :todo-type headline)))
  ;;        (tags (and (plist-get info :with-tags)
  ;;                   (org-export-get-tags headline info)))
  ;;        (priority (and (plist-get info :with-priority)
  ;;                       (org-element-property :priority headline)))
  ;;        ;; Create the headline text.
  ;;        (full-text (if (fboundp 'org-html-format-headline--wrap)
  ;;                       (org-html-format-headline--wrap headline info)
  ;;                     (org-html-headline headline "" info))))
  ;;   (cond
  ;;    ;; Case 1: This is a footnote section: ignore it.
  ;;    ((org-element-property :footnote-section-p headline) nil)
  ;;    ;; Case 2. This is a deep sub-tree: export it as a list item.
  ;;    ;;         Also export as items headlines for which no section
  ;;    ;;         format has been found.
  ;;    ((org-export-low-level-p headline info)
  ;;     ;; Build the real contents of the sub-tree.
  ;;     (let* ((type (if numberedp 'ordered 'unordered))
  ;;            (itemized-body (org-html-format-list-item
  ;;                            contents type nil info nil full-text)))
  ;;       (concat
  ;;        (and (org-export-first-sibling-p headline info)
  ;;             (org-anki-begin-plain-list type))
  ;;        itemized-body
  ;;        (and (org-export-last-sibling-p headline info)
  ;;             (org-anki-end-plain-list type)))))

  ;;    ;; Case 3. Standard headline.  Export it as a section.
  ;;    (t
  ;;     (let* ((section-number (mapconcat 'number-to-string
  ;;                                       (org-export-get-headline-number
  ;;                                        headline info) "-"))
  ;;            (ids (remove 'nil
  ;;                         (list (org-element-property :CUSTOM_ID headline)
  ;;                               (concat "sec-" section-number)
  ;;                               (org-element-property :ID headline))))
  ;;            (level1 (+ level (1- org-html-toplevel-hlevel)))
  ;;            (hlevel (org-anki-get-hlevel info))
  ;;            (first-content (car (org-element-contents headline))))
  ;;       (concat

  ;;        ;; Stop previous slide.
  ;;        ;; FIXME: This will make slide has more </slide> element
  ;;        (if (or (/= level 1)
  ;;                (not (org-export-first-sibling-p headline info)))
  ;;            "</slide>\n")

  ;;        (org-anki-close-element
  ;;         (org-anki--container headline info)
  ;;         ;; container class
  ;;         (org-anki--container-class headline info)
  ;;         ;; body
  ;;         (format "%s%s%s"
  ;;                 ;; aside
  ;;                 (org-anki--aside headline info)
  ;;                 ;; title
  ;;                 (org-anki--title headline info)

  ;;                 ;; When there is no section, pretend there is an empty
  ;;                 ;; one to get the correct <div class="outline- ...>
  ;;                 ;; which is needed by `org-info.js'.
  ;;                 (if (not (eq (org-element-type first-content) 'section))
  ;;                     (concat (org-html-section first-content "" info)
  ;;                             "")
  ;;                   contents)
  ;;                 ))
  ;;        ))))))

(defun org-anki-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  contents)

;; Footnotes

(defun org-anki--footer-from-footnote ()
  "Generate <footer> for footnotes in a single slide page."
  (if org-anki--current-footnote-list
      (prog1 (concat
              "<footer class=\"source\">\n"
              (mapconcat #'identity (reverse org-anki--current-footnote-list) "\n")
              "\n</footer>")
        ;; clean list & footnote count
        (setq org-anki--current-footnote-list nil)
        (setq org-anki--footnote-count-in-slide 0)
        )
    ""))

(defun org-anki-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML (<footer class='source'>).
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   (cond
    ;; Do nothing if footnote has already been defined.
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     "")
    ;; Do nothing if reference is within another footnote
    ;; reference, footnote definition or table cell.
    ((cl-loop for parent in (org-export-get-genealogy footnote-reference)
              thereis (memq (org-element-type parent)
                            '(footnote-reference footnote-definition table-cell)))
     "")
    ;; Otherwise, add it into org-anki--current-footnote-list
    (t
     (let ((def (org-export-get-footnote-definition footnote-reference info)))
       (cl-incf org-anki--footnote-count-in-slide)
       (push
        ;; [FIXME] `replace-regexp' as ugly workaround because I don't
        ;; know how to redefine the back-end for footnote-definition
        (replace-regexp-in-string "<p class=\"footpara\">"
                                  (format "<p class=\"footpara\">%s. " org-anki--footnote-count-in-slide)
                                  (org-trim (org-export-data def info)))
        org-anki--current-footnote-list)
       ;; Add number as <sup>
       (format "<sup>%s</sup>" org-anki--footnote-count-in-slide)
       )))))


(defun org-anki-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents
  )


;;;; Table Cell (support for "highlight" class)
(defun org-anki-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
         (table (org-export-get-parent-table table-cell))
         (cell-attrs
          (if (not org-html-table-align-individual-fields) ""
            (format " class=\"%s%s\""
                    (org-export-table-cell-alignment table-cell info)
                    (if (and (stringp contents)
                             (string-prefix-p "* " contents))
                        (progn (setq contents (substring contents 2))
                               " highlight")
                      "")
                    ))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
           (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-html-table-header-tags) "col" cell-attrs)
              contents (cdr org-html-table-header-tags)))
     ((and org-html-table-use-header-tags-for-first-column
           (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-html-table-header-tags) "row" cell-attrs)
              contents (cdr org-html-table-header-tags)))
     (t (concat "\n" (format (car org-html-table-data-tags) cell-attrs)
                contents (cdr org-html-table-data-tags))))))


;; Plain List

;; FIXME: Maybe arg1 is not needed because <li value="20"> already sets
;; the correct value for the item counter
(defun org-anki-begin-plain-list (type &optional class arg1)
  "Insert the beginning of the HTML list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (if class
      ;; quotes should be removed from "\"build fade\"", so:
      (setq class
            (format " class=\"%s\""
                    (replace-regexp-in-string "[\"']" "" class)))
    (setq class ""))
  (cl-case type
    (ordered
     (format "<ol%s%s>"
             class
             (if arg1 (format " start=\"%d\"" arg1) "")))
    (unordered (format "<ul%s>" class))
    (descriptive (format "<dl%s>" class))))

(defun org-anki-end-plain-list (type)
  "Insert the end of the HTML list depending on TYPE."
  (cl-case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-anki-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attributes (org-export-read-attribute :attr_html plain-list))
         (class (plist-get attributes :class)))
    (format "%s\n%s%s"
            (org-anki-begin-plain-list type class)
            contents (org-anki-end-plain-list type))))



;;; Template
(defun org-anki-template (contents info)
  "Return complete document string after HTML conversion.
contents is the transoded contents string.
info is a plist holding export options."

  ;; Slide contents
  (let* ((ret (replace-regexp-in-string "[\n\t]+" " " contents))
	 (ret (replace-regexp-in-string "<cardseparator/>" "\n" ret))
	 (ret (replace-regexp-in-string "<fieldseparator/>" "\t" ret))
	 (ret (replace-regexp-in-string "^[ ]*\n" "" ret))
	 ;; change &#x2013; to --
	 (ret (replace-regexp-in-string "&#x2013;" "--" ret))
	 (ret (replace-regexp-in-string "&#x2014;" "---" ret))
	 (ret (replace-regexp-in-string "<sub>" "_" ret))
	 (ret (replace-regexp-in-string "</sub>" "" ret))
	 
	 )
    ret))


;;; End-user functions
;;;###autoload
(defun org-anki-make-ids ()
  "this function make ids for all oft the headlines in buffer and
  than ask for saving the buffer"
  (save-excursion
    (goto-char (point-min))
    (while (progn (when (org-at-heading-p)
		    (org-id-get-create))
		  (= 0 (forward-line 1)))))
  (when (buffer-modified-p)
    (when (y-or-n-p "Added IDs, save the buffer?")
      (save-buffer))))
      
      
      
  
(defun org-anki-align-tabs (outbuf)
  ;; this will balance tabs in each line by adding tabs in the end so
  ;; that each line will have the same number of tabs
  (let (tabs-in-lines
	tabs-in-line
	bds
	maxtabs)
    (with-current-buffer outbuf
      (goto-char 1)
      (while (progn
	       (setq tabs-in-line 0)
	       (setq bds (bounds-of-thing-at-point 'line))
	       (while (re-search-forward "\t" (cdr bds) t)
		 (incf tabs-in-line))
	       (push tabs-in-line tabs-in-lines)
	       (forward-line 1)
	       (thing-at-point 'char)))
      (goto-char 1)
      (setq tabs-in-lines (reverse tabs-in-lines))
      (setq maxtabs (apply 'max tabs-in-lines))
      (dolist (tabs-in-line tabs-in-lines)
	(goto-char (line-end-position))
	(dotimes (num (- maxtabs tabs-in-line))
	  (insert "\t"))
	(forward-line 1))))
  outbuf)

(defun org-anki-export-as-csv
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-anki-make-ids)
  (let ((outbuf (org-export-to-buffer 'anki "*Org Anki Export*"
		  async subtreep visible-only body-only ext-plist )))
    ;; add tabs
    (org-anki-align-tabs outbuf)))
    
      
    


;; (defun org-anki-export-as-csviq
;;     (&optional async subtreep visible-only body-only ext-plist)
;;   "Export current buffer to an HTML buffer.

;; Export is done in a buffer named \"*Org HTML5 Slide Export*\", which
;; will be displayed when `org-export-show-temporary-export-buffer'
;; is non-nil."
;;   (interactive)
;;   (let ((outbuf (org-export-to-buffer
;;                     'anki "*Org Anki Export*"
;;                   async subtreep visible-only body-only ext-plist))
;;         (org-export-coding-system org-html-coding-system))
;;     ;; Set major mode.
;;     (with-current-buffer outbuf (set-auto-mode t))
;;     (when org-export-show-temporary-export-buffer
;;       (switch-to-buffer-other-window outbuf)
;;       ;; Indent html buffer to make debug more easy
;;       (delete-trailing-whitespace)
;;       (indent-region (point-min) (point-max))
;;       (untabify (point-min) (point-max)))))

;;;###autoload
(defun org-anki-export-to-csv
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to CSV file"
  (interactive)
  (org-anki-make-ids)
  (let* ((extension (concat "." "txt"))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system)
	 (file (read-file-name "file name: " nil file nil file)))
    ;; Check resource and re-fetch it
    ;;(org-anki-check-resource)
    ;; export to html use anki backend
    (org-export-to-file
        'anki file subtreep visible-only body-only ext-plist)
    (with-temp-file file
      (insert-file-contents file)
      (org-anki-align-tabs (buffer-name)))
    ))


(provide 'ox-anki)
;;; ox-anki.el ends here
