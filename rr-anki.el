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
	    (insert (format "/%s/\n" txt))))))))


(defun rr--deepl-engine (from to alternatives txt)
  "translate txt from language to language and put in the current
buffer when alternatives(Y/n) show alternatives"
  (call-process "/home/rrybanie/tmp/translate-shell/translate" nil t nil (concat from ":" to ) "-e" "deepl" "-show-alternatives" alternatives "-show-original" "n" "-show-languages" "n" "-indent" "0" "-no-ansi" txt ))

(defun rr--deepl-engine-ivy (from to txt)
  "translate txt from language to language and put in the current
buffer, use IVY to select candidate"
  (let (lista)
    (with-temp-buffer
      (call-process "/home/rrybanie/tmp/translate-shell/translate" nil t nil (concat from ":" to ) "-e" "deepl" "-show-alternatives" "Y" "-show-original" "n" "-show-languages" "n" "-indent" "0" "-no-ansi" txt )
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

(defun rr--reverso-get-xml (word)
  ;; connects with the reverso site, gets html and returns parsed xml
  (with-temp-buffer
    (url-insert-file-contents (format "http://context.reverso.net/translation/german-english/%s" word))
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
