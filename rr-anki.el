(require 'google-translate)

(setq rr--cloze-hide-char ?_)

(defun rr--transform-string-to-cloze (txt)
  ;; replace every second character as .
  ;; for txt beschleunigung
  ;; should return .e.c.l.u.i.u.g
  ;; for text shorter than 5 chars, make x___
  (if (> (length txt) 4)
      (let* ((counter 0)
	     (chtext (mapconcat (lambda (x) (if (= counter 0)
						(progn
						  (setq counter 1)
						  (make-string 1 rr--cloze-hide-char))
					      (progn
						(setq counter 0)
						(string x)
						)))
				txt "" )))
	(aset chtext (- (length chtext) 1) rr--cloze-hide-char)
	chtext)
    ;; else length <= 4
    (let ((ret (make-string (length txt) rr--cloze-hide-char)))
      (aset ret 0 (elt txt 0))
      ret)))



(defun rr--my-paragraph-bound ()
  ;; in org, header is not treated as paragraph
  ;; when we are ine header we take positoin of the header START
  ;; and check if there is following paraphraph
  ;; if there is paragraph we will use end position of the paragraph as END
  (save-excursion
    (if (org-at-heading-p)
	(bounds-of-thing-at-point 'line)
      (let ((bds (bounds-of-thing-at-point 'paragraph)))
	(if bds
	    (progn
	      (forward-line -1)
	      (cons (if (bounds-of-thing-at-point 'paragraph)
			(car bds)
		      (car (bounds-of-thing-at-point 'line))) (cdr bds)))
	  ;; else
	  (setq bds (bounds-of-thing-at-point 'line))
	  (forward-line)
	  (cons (car bds) (if (bounds-of-thing-at-point 'paragraph)
			      (cdr (bounds-of-thing-at-point 'paragraph))
			    (cdr bds))))))))

;;; cloze creation
(defun rr-transform-to-cloze (uarg)
  "change active region to cloze"
  "universal argument say which cARG in cloze"
  (interactive "P")
  (message "%s" current-prefix-arg)
  (let (markstart markend bds)
    (if (use-region-p)
	(setq markstart (region-beginning) markend (region-end))
      (setq bds (bounds-of-thing-at-point 'word))
      (setq markstart (car bds) markend (cdr bds)))
    
    (let ((txt (buffer-substring markstart markend)))
      (goto-char markend)
      (insert "::")
      (insert (rr--transform-string-to-cloze txt))
      (insert "}}")
      (goto-char markstart)
      (message "uarg is %s" uarg)
      (setq bds (rr--my-paragraph-bound))
      (cond
       ((equal uarg nil)
	(insert (format "{{c%d::" (rr--transform-to-cloze-get-free-c (car bds) (cdr bds)))))
       ((equal uarg '(4))
	(insert (format "{{c%d::" (rr--transform-to-cloze-get-highest-c (car bds) (cdr bds)))))
       (t
	(insert (format "{{c%d::" uarg)))
       (message "text is %s" txt)))))


(put 'erase-buffer 'disabled nil)


(defun rr--transform-to-cloze-get-free-c (startpoint endpoint)
  "look for the {{cx:: and returns first free x as int"
  (save-excursion
    (goto-char startpoint)
    (let (numlist (minimal 1))
      (while (re-search-forward "{{c\\([0-9]+\\)::" endpoint t)
	(push (string-to-number(match-string 1))  numlist))
      ;; now get minimal element
      (if (not numlist)
	  1
	(while (memq minimal numlist) ;;member memq
	  (incf minimal))
	minimal))))

(defun rr--transform-to-cloze-get-highest-c (startpoint endpoint)
  "look for the {{cx:: and returns biggest found x as int"
  (save-excursion
    (goto-char startpoint)
    (let (numlist (minimal 1))
      (while (re-search-forward "{{c\\([0-9]+\\)::" endpoint t)
	(push (string-to-number(match-string 1))  numlist))
      ;; now get minimal element
      (message "%s" numlist)
      (seq-max numlist))))

(defun rr--put-in-subheading (txt)
  (save-excursion
    (when (re-search-backward "^\\*[ ]" nil t) ;; find top heading
      (forward-char)
      (when (re-search-forward "^\\*+[ ]" nil t)
	(backward-char)
	(if (equal (char-before) ?*)
	    (progn 
	      (if (re-search-forward "^\\*+[ ]" nil t)
		  (progn
		    (forward-line -1)
		    (forward-line))
		;; last line
		(goto-char (point-max))))
	  ;;else
	  (forward-line -1)
	  (end-of-line)
	  (insert (format "\n** \n")))
	;; check if we need to add newline before
	(forward-line -1)
	(unless (re-search-forward "^[ ]*$" (line-end-position) t)
	  (forward-line)
	  (insert "\n")))
      (insert (format "%s\n" txt)))))

(defun rr--put-after-drawer (txt)
  (while (and (= 0 (forward-line 1))
	      (or (org-at-drawer-p) (org-at-property-p))))
  (insert (format "%s\n" txt)))

(defun rr--translate-sentence (uarg engine)
  "make engine to translate highlighted region or paragraph
   automaticaly put text below or copy to kill-buffer when ARG
   engine takes one argument: txt to translate, should produce
   results in current buffer"
  ;;(message "%s" current-prefix-arg)
  (let (markstart markend bds)
    (if (use-region-p)
	(setq markstart (region-beginning) markend (region-end))
      (setq bds (rr--my-paragraph-bound))
      (setq markstart (car bds) markend (- (cdr bds) 1)))
    (when (and markstart markend)
      (let ((txt (buffer-substring markstart markend)))
	(when (with-temp-buffer
		(insert txt)
		(goto-char (point-min))
		(while (re-search-forward "\\(<field/>\\)\\|\\(<card/>\\)\\|\n" nil 1)
		  (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "{{c[0-9]+::\\(.+?\\)\\(::.*?}}\\)\\|\\(}}\\)" nil 1)
		  (replace-match "\\1"))
		(goto-char (point-min))
		(while (re-search-forward "^\\*" nil 1)
		  (replace-match ""))
		(setq txt (buffer-substring (point-min) (point-max)))
		(erase-buffer)
		(funcall engine txt)
		(if uarg
		    (progn 
		      (kill-region (point-min) (point-max))
		      (message "Translated text is in the kill buffer")
		      nil)
		  (setq txt (buffer-substring (point-min) (point-max)))
		  ;; (message (format "txt is %s" txt)))
		))
	  ;; we enter here only when uarg
	  (goto-char markend)
	  (when (> (length txt) 1)
	    (let ((level (org-current-level))
		  (stars ""))
	      ;; add new entry
	      ;; (while (and (= 0 (forward-line 1))
	      ;; 		  (not (org-at-heading-p))))
	      (while (> level 0)
		(decf level)
		(setq stars (concat stars "*")))
	      (rr--put-after-drawer (concat stars "* " txt))
	      ;;(insert "* ")
	      ;;(insert (format "%s\n\n" txt))
	      ;;(org-insert-heading-respect-content)
	      ;;(org-toggle-heading)
	      ;;(insert (format "/%s/\n" txt))
	      )))))))


(defun rr--deepl-engine (from to alternatives txt)
  "translate txt from language to language and put in the current
buffer when alternatives(Y/n) show alternatives"
  (call-process "/usr/bin/trans" nil t nil (concat from ":" to ) "-e" "deepl" "-show-alternatives" alternatives "-show-original" "n" "-show-languages" "n" "-indent" "0" "-no-ansi" txt ))

(defun rr--deepl-engine-ivy (from to txt)
  "translate txt from language to language and put in the current
buffer, use IVY to select candidate"
  (let (lista)
    (with-temp-buffer
      (call-process "/usr/bin/trans" nil t nil (concat from ":" to ) "-e" "deepl" "-show-alternatives" "Y" "-show-original" "n" "-show-languages" "n" "-indent" "0" "-no-ansi" txt )
      (goto-char (point-min))
      (let ((breakloop 0))
	(while (= breakloop 0)
	  ;;(message "%s" (buffer-substring-no-properties  (line-beginning-position) (line-end-position)))
	  (setq lista (append lista (list
				     (concat (buffer-substring-no-properties  (line-beginning-position) (line-end-position))
					     ""))))
	  (setq breakloop (forward-line 1)))))
    (insert (ivy-read (format "Translation of: %s" "aa") lista))))


;;; user ineteractive functnions
(defun rr-google-translate (uarg)
  "translate txt and put in the current buffer"
  (interactive "P")
  (rr--translate-sentence uarg (lambda (txt) (google-translate-translate "de" "pl" txt 'current-buffer))))

(defun rr-google-translate-en (uarg)
  "translate txt and put in the current buffer"
  (interactive "P")
  (rr--translate-sentence uarg (lambda (txt) (google-translate-translate "de" "en" txt 'current-buffer))))

(defun rr-deepl-translate (uarg)
  "translate txt and put in the current buffer"
  (interactive "P")
  (rr--translate-sentence uarg (lambda (txt) (rr--deepl-engine-ivy "de" "pl" txt))))


(defun rr--depl-lookup-word-gen-list (word)
  ;; (get-buffer-create "workingxml")
  ;; (set-buffer "workingxml")
  ;; (erase-buffer)
  (when (> (length word) 3)
    (with-temp-buffer
      (call-process "grep" nil t nil word (concat (getenv "HOME") "/" "depl.xml") "-i")
      (goto-char (point-min))
      (let ((breakloop 0)
	    (lista nil))
	(while (= breakloop 0)
	  ;;(message "%s" (buffer-substring-no-properties  (line-beginning-position) (line-end-position)))
	  (setq lista (append lista (list
				     (concat (buffer-substring-no-properties  (line-beginning-position) (line-end-position))
					     ""))))
	  (setq breakloop (forward-line 1)))
	lista))))


(defun rr-counsel-depl-lookup-word (markstart markend)
  "generate list of text from the dictionary"
  (interactive "r")

  (let ((initword (word-at-point)))
    (when (use-region-p)
      ;; we started with highlighted region
      (setq initword (buffer-substring-no-properties markstart markend)))
    (ivy-read "Select word (at least 4 chars: " 'rr--depl-lookup-word-gen-list
	      :caller 'rr-counsel-depl-lookup-word
	      :dynamic-collection t
	      :initial-input initword
	      :action #'(lambda (selected)
			  (kill-new selected)
			  (message "copied to kill ring")))))

(defun rr-cousel-depl-lookup-word-put-subheading (markstart markend)
  "get dictionary result and put in org subheading"
  (interactive "r")
  (rr-counsel-depl-lookup-word markstart markend)
  (rr--put-in-subheading (with-temp-buffer
			  (yank)
			  (buffer-substring-no-properties (point-min) (point-max)))))
  
;; reverso context

(defun rr--reverso-get-xml (word lang)
  ;; connects with the reverso site, gets html and returns parsed xml
  (let ((strlang (cond ((or (equal lang "english")
			    (equal lang "en"))
			"english")
		       ((or (equal lang "polish")
			    (equal lang "pl"))
			"polish")
		       (t
			nil))))
       (when strlang
	 (with-temp-buffer
	   (url-insert-file-contents (format "http://context.reverso.net/translation/german-%s/%s" strlang word))
	   (libxml-parse-html-region (point-min) (point-max))))))

(defun rr--reverso-clean-text (lst)
  ;; get list of stings, combine and remove newlines and double spaces
  (let* ((ret (apply 'concat lst))
	 (ret (replace-regexp-in-string "\n" " " ret))
	 (ret (replace-regexp-in-string " [ ]+" " " ret))
	 (ret (replace-regexp-in-string "^ " "" ret))
	 (ret (replace-regexp-in-string " $" "" ret))
	 )
    ret))
  

(defun rr--reverso-process-xml (lst)
  ;; musimy sprawdzic czy lista i sprawdzic pierwszy tag i ewentulanie sprawdzic czy nie nil
  ;; jesli tag jest div itd lub src to odpalamy helper
  ;; jesli nie to mapcar na liscie elementow tagu (cddr lst)
  ;; zwracamy listę zdetekowanych tekstów lub nil
  (when (and lst (listp lst))
    ;; test for div, trg ltr, src ltr
    (if (and (eq 'div (car lst))
	     (or (equal (cdar (second lst)) "src ltr")
		 (equal (cdar (second lst)) "trg ltr")))
	;;(message "%S" (rr--reverso-parse-helper (cddr lst))))
	(list (rr--reverso-clean-text(rr--reverso-parse-helper (cdr lst))))
      ;; else
      (mapcan #'(lambda (x) (rr--reverso-process-xml x))
	      (cddr lst)))))

(defun rr--reverso-parse-helper (lst)
  ;; this function flattens structure and taking only cddr of tags
  (if (listp lst)
      (mapcan #'(lambda (x)
		  (if (listp x)
		      (rr--reverso-parse-helper x)
		    (list x)))
	      (cddr lst))
    (list x)))

(defun rr--reverso-prepare-candidates (word lang)
  ;; functnion takes word and prepares candidates list from reverso context
  

  ;; we have pairs so we have to group pairs and add newline in between
  (let ((senlst (rr--reverso-process-xml (rr--reverso-get-xml word lang)))
	(candlst nil))
    (while senlst
      (push (format "%s\n/%s/" (pop senlst) (pop senlst)) candlst))
    candlst))


(defvar rr-reverso-language "pl"
  "language for reverso context")

(defun rr-counsel-reverso-lookup-word (markstart markend)
  "generate list of text from the reverso net"
  (interactive "r")
  (let ((initword (word-at-point)))
    (when (use-region-p)
      ;; we started with highlighted region
      (setq initword (buffer-substring-no-properties markstart markend)))
    (setq initword (read-from-minibuffer "Reverso context lookup: " initword))
    (ivy-read "Select: "  (rr--reverso-prepare-candidates initword rr-reverso-language)
	      :caller 'rr-counsel-reverso-lookup-word
	      :dynamic-collection nil
	      :initial-input initword
	      :action #'(lambda (selected)
			  (kill-new selected)
			  (message "copied to kill ring")))))

(defun rr-org-counsel-reverso-lookup-word (markstart markend)
  "generate list of text from the reverso net"
  (interactive "r")
  (let ((initword (word-at-point)))
    (when (use-region-p)
      ;; we started with highlighted region
      (setq initword (buffer-substring-no-properties markstart markend)))
    (setq initword (read-from-minibuffer "Reverso context lookup: " initword))
    (ivy-read "Select: "  (rr--reverso-prepare-candidates initword rr-reverso-language)
	      :caller 'rr-counsel-reverso-lookup-word
	      :dynamic-collection nil
	      :initial-input initword
	      :action #'(lambda (selected)
			  (kill-new (replace-regexp-in-string
				     "\\([[:print:]]*\\)\n/\\([[:print:]]*\\)/" "* \\1 \n\n** \\2 " selected))
			  (message "copied to kill ring")))))


(defun rr-org-put-ids ()
  "generate ids in the buffer for all sub*sections"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (progn
	     (when (org-at-heading-p)
	       (org-id-get-create))
	     (= 0 (forward-line 1))))))
      
(defun rr--org-get-lang-for-tts ()
  "get the language for the speech synthesis based on attribute TTSLANG or level"
  (cdar (org-entry-properties nil "TTSLANG")))
  ;; (let ((lang (or (cdar (org-entry-properties nil "TTSLANG"))
  ;; 		  (if (= (org-current-level) 1)
  ;; 		      "de"
  ;; 		    "pl"))))
  ;;   lang))


(defun rr--get-paragraph-as-text ()
  "get text for current paragraph or marked region, remove tags"
  ;;(message "%s" current-prefix-arg)
  (let (markstart markend bds)
    (if (use-region-p)
	(setq markstart (region-beginning) markend (region-end))
      (setq bds (rr--my-paragraph-bound))
      (setq markstart (car bds) markend (cdr bds)))
    (when (and markstart markend)
      (let ((txt (buffer-substring markstart markend)))
	(with-temp-buffer
	  (insert txt)
	  (goto-char (point-min))
	  (while (re-search-forward "\\(<field/>\\)\\|\\(<card/>\\)\\|\n" nil 1)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "{{c[0-9]+::\\(.+?\\)\\(::.*?}}\\)\\|\\(}}\\)" nil 1)
	    (replace-match "\\1"))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\*" nil 1)
	    (replace-match ""))
	  ;; return txt
	  (setq txt (buffer-substring (point-min) (point-max))))))))


(defun rr--generate-tts ()
  "generate tts for current heading"
  (let* ((txt (rr--get-paragraph-as-text))
	 (id (org-id-get-create))
	 (fname (concat id ".mp3"))
	 (lang (rr--org-get-lang-for-tts))
	 (speed (if (equal lang "pl")
		    "1.5"
		  "1.1"))
	 (mediadir (rr-anki--media-dir (buffer-file-name)))
;;	 (command (format "gtts-cli -l %s \"%s\" | mpv - --speed=%s -o %s/%s" lang (substring-no-properties txt) speed mediadir fname))
	 (command (format "gtts-cli -l %s \"%s\" | ffmpeg -y -i - -filter:a \"atempo=%s\" -vn %s/%s" lang (substring-no-properties txt) speed mediadir fname)))
    (when (and (buffer-file-name)
	       lang)
      (unless (file-directory-p mediadir)
	(make-directory mediadir))
      (when (process-lines "/bin/bash" "-c" command)
	fname))))

(defun rr-generate-tts ()
  "generate tts for current heading, if not already generated,
with universal argument always regenerate"
  (interactive)
  (unless (buffer-file-name)
    (when (y-or-n-p ("You have to save the buffer first. Save it?"))
      (save-buffer)))
  (when (buffer-file-name)
    (when (or current-prefix-arg
	      (not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" (org-id-get-create) ".mp3"))))
      (rr--generate-tts))))

(defun rr-anki-play ()
  "play current mp3 based on ID"
  (interactive)
  (unless (buffer-file-name)
    (when (y-or-n-p "You have to save the buffer first. Save it?")
      (save-buffer)))
  (when (buffer-file-name)
    (let ((fname (concat (rr-anki--media-dir (buffer-file-name)) "/" (org-id-get-create) ".mp3")))
      (if (file-exists-p fname)
	  (process-lines "/usr/bin/mpv" fname)
	(message "File %s does not exist" fname)))))
    

(defun rr-org-next-h (level)
  (let ((heading-found nil))
    (while (and (not heading-found)
		(= 0 (forward-line 1)))
      (when (and  (org-at-heading-p)
		  (= level (org-current-level)))
	(setq heading-found t)))
    heading-found))

(defun rr-org-next-h-in-subtree (level)
  (let ((heading-pos nil)
	(subtree-ok nil))
    (save-excursion
      (while (and (not heading-pos)
		  (= 0 (forward-line 1)))
	(when (and (org-at-heading-p)
		   (> level (org-current-level)))
	  (setq heading-pos (point))
	  (setq subtree-ok nil))
	(when (and  (org-at-heading-p)
		    (= level (org-current-level)))
	  (setq heading-pos (point))
	  (setq subtree-ok t))))
    (when subtree-ok
      (goto-char heading-pos))))



(defun rr-org-next-h2-after-h1 ()
  (when  (rr-org-next-h 1)
    (rr-org-next-h 2)))

(defun rr-org-set-ttt-lang-in-buffer ()
  "set language for sections and firs subsection"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (rr-org-next-h-in-subtree 1)
      (org-id-get-create)
      (unless (cdar (org-entry-properties nil "TTSLANG"))
	(org-set-property "TTSLANG" "de"))
      (when (rr-org-next-h-in-subtree 2)
	(org-id-get-create)
	(unless (cdar (org-entry-properties nil "TTSLANG"))
	  (org-set-property "TTSLANG" "pl"))))))

(defun rr-org-genarate-tts-in-buffer ()
  "generate tts mp3s for the whole buffer"
  (if (not (buffer-file-name))
      (message "Save buffer first!")
    (save-excursion
      (let ((cnt-hdr 0)
	    (tts-cnt-hdr 0)
	    (cnt-shdr 0)
	    (tts-cnt-shdr 0)
	    (tts-cnt 0))
	(goto-char (point-min))
	(while (rr-org-next-h-in-subtree 1)
	  (incf cnt-hdr)
	  (when (cdar (org-entry-properties nil "TTSLANG"))
	    (incf tts-cnt-hdr))
	  (when (rr-org-next-h-in-subtree 2)
	    (incf cnt-shdr)
	    (when (cdar (org-entry-properties nil "TTSLANG"))
	      (incf tts-cnt-shdr))))
	(message "Headers: %d TTS: %d\nSubHeaders: %d TTS: %d" cnt-hdr tts-cnt-hdr cnt-shdr tts-cnt-shdr)
	(goto-char (point-min))
	(while (rr-org-next-h-in-subtree 1)
	  (when (cdar (org-entry-properties nil "TTSLANG"))
	    (message "Generating TTS %d/%d" (incf tts-cnt) (+ tts-cnt-hdr tts-cnt-shdr))
	    (rr-generate-tts))
	  (when (rr-org-next-h-in-subtree 2)
	    (when (cdar (org-entry-properties nil "TTSLANG"))
	      (message "Generating TTS %d/%d" (incf tts-cnt) (+ tts-cnt-hdr tts-cnt-shdr))
	      (rr-generate-tts))))
	  
	  
	  ;; (while (progn (when (and (org-at-heading-p) (= (org-current-level) 1))
	  ;; 		(incf cnt-hdr)
	  ;; 		(when (cdar (org-entry-properties nil "TTSLANG"))
	  ;; 		  (incf tts-cnt-hdr))
	  ;; 		;; now find first subheading
	  ;; 		(while (and (= 0 (forward-line 1))
	  ;; 			    (not (org-at-heading-p))))
	  ;; 		(if (and (org-at-heading-p) (= (org-current-level) 2))
	  ;; 		    (progn
	  ;; 		      (incf cnt-shdr)
	  ;; 		      (when (cdar (org-entry-properties nil "TTSLANG"))
	  ;; 			(incf tts-cnt-shdr)))
	  ;; 		  (forward-line -1)))
	  ;; 	      (= 0 (forward-line 1))))
	  ;; (message "Headers: %d TTS: %d\nSubHeaders: %d TTS: %d" cnt-hdr tts-cnt-hdr cnt-shdr tts-cnt-shdr)
	  ;; (goto-char (point-min))
	  ;; (while (progn (when (and (org-at-heading-p) (= (org-current-level) 1))
	  ;; 		(when (cdar (org-entry-properties nil "TTSLANG"))
	  ;; 		  (message "Generating TTS %d/%d" (incf tts-cnt) (+ tts-cnt-hdr tts-cnt-shdr))
	  ;; 		  (rr-generate-tts))
	  ;; 		;; now find first subheading
	  ;; 		(while (and (= 0 (forward-line 1))
	  ;; 			    (not (org-at-heading-p))))
	  ;; 		(if (and (org-at-heading-p) (= (org-current-level) 2))
	  ;; 		    (when (cdar (org-entry-properties nil "TTSLANG"))
	  ;; 		      (message "Generating TTS %d/%d" (incf tts-cnt) (+ tts-cnt-hdr tts-cnt-shdr))
	  ;; 		      (rr-generate-tts))
	  ;; 		  (forward-line -1)))
	  ;; 	      (= 0 (forward-line 1))))

	  
	  ))))


(defhydra hydra-rr-org ()
  "RR Anki org"
  ("n" (rr-org-next-h2-after-h1) "next h2 after h1")
  ("N" (rr-org-next-h 1) "next h1")
  ("d" (progn (org-set-property "TTSLANG" "de") (org-id-get-create)) "set TTS de")
  ("o" (progn (org-set-property "TTSLANG" "pl") (org-id-get-create)) "set TTS pl")
  ("e" (progn (org-set-property "TTSLANG" "en") (org-id-get-create)) "set TTS en")) 


(defun rr-anki--media-dir (bufname)
  "Get the directory name from the base file"
  (when bufname
    (setq bufname (replace-regexp-in-string "\\([[:print:]]+\\)\\.org" "\\1.media" bufname))))


(provide 'rr-anki)
