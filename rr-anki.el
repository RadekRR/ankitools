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
			  (cdr bds)))))))
						      
	
      
	 

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

(defun rr--google-translate (uarg fromlang tolang)
  "make google translate for highlighted region or paragraph
   automaticaly put text below or copy to kill-buffer when ARG"
  (message "%s" current-prefix-arg)
  (let (markstart markend bds)
    (if (use-region-p)
	(setq markstart (region-beginning) markend (region-end))
      (setq bds (rr--my-paragraph-bound))
      (setq markstart (car bds) markend (cdr bds)))
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
		(setq txt (buffer-substring (point-min) (point-max)))
		(erase-buffer)
		(google-translate-translate fromlang tolang txt 'current-buffer)
		(if uarg
		    (progn 
		      (kill-region (point-min) (point-max))
		      (message "Translated text is in the kill buffer")
		      nil)
		  (setq txt (buffer-substring (point-min) (point-max)))
		  (message "")))
	  ;; we enter here only when uarg
	  (goto-char markend)
	  (when (> (length txt) 1)
	    (insert (format "/%s/\n" txt))))))))

(defun rr-google-translate (uarg)
  "translate txt and put in the current buffer"
  (interactive "P")
  (rr--google-translate uarg "de" "pl"))

(defun rr-google-translate-en (uarg)
  "translate txt and put in the current buffer"
  (interactive "P")
  (rr--google-translate uarg "de" "en"))


(defun lookup--word-gen-list (word)
  ;; (get-buffer-create "workingxml")
  ;; (set-buffer "workingxml")
  ;; (erase-buffer)
  (when (> (length word) 3)
    (with-temp-buffer
      (call-process "grep" nil t nil word "/home/rrybanie/depl.xml" "-i")
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


(defun counsel-lookup-word (markstart markend)
  "generate list of text from the dictionary"
  (interactive "r")

  (let ((initword (word-at-point)))
    (when (use-region-p)
      ;; we started with highlighted region
      (setq initword (buffer-substring-no-properties markstart markend)))
    (ivy-read "Select word (at least 4 chars: " 'lookup--word-gen-list
	      :caller 'counsel-lookup-word
	      :dynamic-collection t
	      :initial-input initword
	      :action #'(lambda (selected)
			  (kill-new selected)
			  (message "copied to kill ring")))))

;; (defun rr-export-to-anki (filename)
;;   "export cards to an csv file for import with Anki"
;;   (interactive "F")
;;   (get-buffer-create "workingxml")
;;   (copy-to-buffer "workingxml" (point-min) (point-max))
;;   (set-buffer "workingxml")
;;   ;;(erase-buffer)

;;   (goto-char (point-min))
;;   (let ((breakloop 0)
;; 	(last-head "X")
;; 	head)
;;     (while (and (= breakloop 0)
;; 		(> (- (point-max) (line-beginning-position)) 2))
;;       (setq head (buffer-substring-no-properties (line-beginning-position) (+ 2 (line-beginning-position))))
;;       (cond ((string= "##" head)
;; 	     (goto-char (line-beginning-position))
;; 	     (delete-char 2)
;; 	     (insert "<div>")
;; 	     (goto-char (line-end-position))
;; 	     (insert "</div>")
;; 	     (when (string= last-head "#")
;; 	       (goto-char (line-beginning-position))
;; 	       (delete-char -1))
;; 	     (setq last-head "#"))
;; 	    ((string= "@@" head)
;; 	     (goto-char (line-beginning-position))
;; 	     (delete-char 2)
;; 	     (insert "<div><i>")
;; 	     (goto-char (line-end-position))
;; 	     (insert "</i></div>")
;; 	     (unless (string= last-head "@")
;; 	       (insert "\t")
;; 	       (setq last-head "@"))
;; 	     (goto-char (line-beginning-position))
;; 	     (delete-char -1))
;; 	    ((string= "==" head)
;; 	     (goto-char (line-beginning-position))
;; 	     (delete-char 2)
;; 	     (insert "<div>")
;; 	     (goto-char (line-end-position))
;; 	     (insert "</div>")
;; 	     (unless (string= last-head "=")
;; 	       (insert "\t")
;; 	       (setq last-head "="))
;; 	     (goto-char (line-beginning-position))
;; 	     (delete-char -1))
;; 	    (t
;; 	     (message "Error: header unrecognized in line %d" (line-number-at-pos (point)))))
;;       (setq breakloop (forward-line 1)))
;;     ))






;; (defun walk (node)
;;   (if (not (listp node))
;;       ;;  (message "%s" node)
;;       (if (string= "\n" node)
;; 	  ()
;; 	(insert node))
;;     ;; else
;;     (let ((point-adv 0))
;;       (if (string= "div" (xml-node-name node))
;; 	  (insert "\n")
;; 	(cond ((string= "bold" (xml-get-attribute node 'class))
;; 	       (insert "")
;; 	       (backward-char 0)
;; 	       (setq point-adv 0))
;; 	      ((string= "underline" (xml-get-attribute node 'class))
;; 	       (insert "<u></u>")
;; 	       (backward-char 4)
;; 	       (setq point-adv 4))
;; 	      ((string= "italic" (xml-get-attribute node 'class))
;; 	       (insert "<i></i>")
;; 	       (backward-char 4)
;; 	       (setq point-adv 4))))
;;       (mapc 'walk (xml-node-children node))
;;       (forward-char point-adv))))

;; (defun blek ()
;;   "this function parses the output from the caluibre to make list of entries from depl dictionary"
;;   (get-buffer-create "workingxml")
;;   (set-buffer "workingxml")
;;   (erase-buffer)
;;   (let ((xxx (xml-parse-file "/home/rrybanie/depl.html")))
;;     (when (not xxx)
;;       (message "Error")
;;       (message "%s" nxml-file-parse-error))
;;     ;;(message "Lista: %s" xxx)
;;     (walk (car xxx)))
;;   (goto-char (point-min))
;;   (delete-region (point-min) (- (search-forward "<u>Aachen" nil 1) 9)))



;; reverso context

(defun rr--reverso-get-xml (word)
  ;; connects with the reverso site, gets html and returns parsed xml
  (with-temp-buffer
    (url-insert (url-retrieve-synchronously (format "http://context.reverso.net/translation/german-english/%s" word)))
    (libxml-parse-html-region (point-min) (point-max))))

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

(defun rr--reverso-prepare-candidates (word)
  ;; functnion takes word and prepares candidates list from reverso context
  

  ;; we have pairs so we have to group pairs and add newline in between
  (let ((senlst (rr--reverso-process-xml (rr--reverso-get-xml word)))
	(candlst nil))
    (while senlst
      (push (format "%s\n/%s/" (pop senlst) (pop senlst)) candlst))
    candlst))


;; (setq testhtml
;;       '(html ((lang . "en") (dir . "ltr"))
;; 	     (head nil
;; 		   (body nil
;; 			 (div ((id . "OPENSUBTITLES-2016.DE-EN_10843473") (class . "example")) "      "
;; 			      (div ((class . "src ltr")) "        " (span ((class . "text") (lang . "de")) "          Ich werde meinen Freund " (em nil "Abelard") " bitten, es wieder zu übermalen.") "      ") "      "
			      
;; 			      (div ((class . "trg ltr")) "        " (span ((class . "icon jump-right"))) "        " (span ((class . "text")) "          I'll have my friend " (a ((class . "link_highlighted") (href . "/translation/english-german/Abelard") (rel . "nofollow")) (em nil "Abelard")) " come back and paint over it.") "      ") "      " 
;; 			      (div ((class . "options")) "        " 
;; 				   (div ((class . "src")) "          " (button ((data-id . "OPENSUBTITLES-2016.DE-EN_10843473") (class . "voice icon stopped") (title . "Pronunciation") (data-lang . "de"))) "          ") "        " 
;; 				   (div ((class . "trg")) "          " (button ((class . "icon more-context") (title . "See this translation example in its context (http://opus.lingfil.uu.se/OpenSubtitles2016.php)") (data-id . "OPENSUBTITLES-2016.DE-EN_10843473"))) "          " (button ((class . "report icon thumb-down") (title . "Report a problem in this example") (data-id . "OPENSUBTITLES-2016.DE-EN_10843473"))) "          " (button ((class . "add icon addentry ") (title . "Add this translation to Reverso Collaborative Dictionary") (data-url . "http://dictionary.reverso.net/CollabDict.aspx?view=2&lang=EN") (data-id . "OPENSUBTITLES-2016.DE-EN_10843473") (data-text . "I'll have my friend <em>Abelard</em> come back and paint over it."))) "          " (button ((class . "copy icon mobile-hidden") (title . "Copy the translation") (data-id . "OPENSUBTITLES-2016.DE-EN_10843473"))) "          " (button ((class . "icon non-favourite ") (title . "Mark this example as favourite") (data-id . "OPENSUBTITLES-2016.DE-EN_10843473"))) "          " (button ((data-id . "OPENSUBTITLES-2016.DE-EN_10843473") (class . "voice icon stopped") (title . "Pronunciation") (data-lang . "en"))) "          ") "      ") "      " (button ((data-id . "OPENSUBTITLES-2016.DE-EN_10843473") (class . "mobile-voice icon stopped played"))) "      ")))))


;; (rr--reverso-process-xml testhtml)


(defun rr-counsel-reverso-lookup-word (markstart markend)
  "generate list of text from the reverso net"
  (interactive "r")
  (let ((initword (word-at-point)))
    (when (use-region-p)
      ;; we started with highlighted region
      (setq initword (buffer-substring-no-properties markstart markend)))
    (setq initword (read-from-minibuffer "Reverso context lookup: " initword))
    (ivy-read "Select: "  (rr--reverso-prepare-candidates initword)
	      :caller 'rr-counsel-reverso-lookup-word
	      :dynamic-collection nil
	      :initial-input initword
	      :action #'(lambda (selected)
			  (kill-new selected)
			  (message "copied to kill ring")))))
(provide 'rr-anki)
