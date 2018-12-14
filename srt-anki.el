(provide 'srt-anki)
(setq srt-anki--regexp "^\\([0-9]+\\)
\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\) --> \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)
\\([[:print:]
]+?\\)

")

(defstruct sub
  start-time
  end-time
  text
  id)


(defun srt-anki--transform-entry (subtxt)
  "return property list from one entry of srt subtitle"
  (with-temp-buffer
    (insert subtxt)
    (goto-char (point-min))
    (when (search-forward-regexp srt-anki--regexp nil t) ;; or string-match
      (let ((id   (string-to-number (match-string 1)))
            (fhrs (string-to-number (match-string 2)))
	        (fmin (string-to-number (match-string 3)))
	        (fsec (string-to-number (match-string 4)))
	        (fmec (string-to-number (match-string 5)))

	        (thrs (string-to-number (match-string 6)))
	        (tmin (string-to-number (match-string 7)))
	        (tsec (string-to-number (match-string 8)))
	        (tmec (string-to-number (match-string 9)))

	        (subt (match-string 10)))
        (make-sub :start-time (+ (* fhrs 60 60) (* fmin 60) fsec (/ fmec 1000.0))
                  :end-time (+ (* thrs 60 60) (* tmin 60) tsec (/ tmec 1000.0))
                  :text subt
                  :id id)))))

;; (defun srt-anki--transform-entry (subtxt)
;;   "return property list from one entry of srt subtitle"
;;   (with-temp-buffer
;;     (insert subtxt)
;;     (goto-char (point-min))
;;     (when (search-forward-regexp srt-anki--regexp nil t) ;; or string-match
;;       (let ((id   (string-to-number (match-string 1)))
;;             (fhrs (string-to-number (match-string 2)))
;; 	        (fmin (string-to-number (match-string 3)))
;; 	        (fsec (string-to-number (match-string 4)))
;; 	        (fmec (string-to-number (match-string 5)))

;; 	        (thrs (string-to-number (match-string 6)))
;; 	        (tmin (string-to-number (match-string 7)))
;; 	        (tsec (string-to-number (match-string 8)))
;; 	        (tmec (string-to-number (match-string 9)))

;; 	        (subt (match-string 10))
;; 	        (retlist (list 'tstart nil 'tend nil 'substr nil)))
;; 	    (setq retlist (plist-put retlist 'tstart (+ (* fhrs 60 60) (* fmin 60) fsec (/ fmec 1000.0))))
;; 	    (setq retlist (plist-put retlist 'tend (+ (* thrs 60 60) (* tmin 60) tsec (/ tmec 1000.0))))
;; 	    (setq retlist (plist-put retlist 'substr subt))
;;         (setq retlist (plist-put retlist 'subid id))))))

(defun srt-anki--retransform-entry (sub)
  "retrun string from retlist => reverse of srt-anki--transform-entry"
  (unless sub
    return nil)
  (with-temp-buffer
    (let* ((id (sub-id sub))
           (substr (sub-text sub))
           (tstart (sub-start-time sub))
           (fhrs (floor (/ tstart 3600)))
           (tstart (- tstart (* 3600 fhrs)))
           (fmin (floor (/ tstart 60)))
           (tstart (- tstart (* 60 fmin)))
           (fsec (floor tstart))
           (fmec (floor (* 1000 (mod tstart 1.0))))

           (tend (sub-end-time sub))
           (thrs (floor (/ tend 3600)))
           (tend (- tend (* 3600 thrs)))
           (tmin (floor (/ tend 60)))
           (tend (- tend (* 60 tmin)))
           (tsec (floor tend))
           (tmec (floor (* 1000 (mod tend 1.0)))))
      (insert
       (format "%d\n" id)
       (format "%02d:%02d:%02d,%03d --> %02d:%02d:%02d,%03d\n" fhrs fmin fsec fmec thrs tmin tsec tmec)
       (format "%s\n" substr)))
    (buffer-substring-no-properties (point-min) (point-max))))
      
(setq testtxt "1194
02:10:10,544 --> 02:10:16,717
Tym, który z otchłani mocy powrócił,
jest twój stary mistrz.

")

(defun srt-anki--parse-file (srtname)
  "parses the srtname file and returns list of sub structures of all entries"
  (with-temp-buffer
    (insert-file-contents srtname)
    (goto-char (point-min))
    (let (retlist)
      (while (search-forward-regexp srt-anki--regexp nil t)
	    ;;(setq retlist (append (srt-anki--transform-entry (match-string 0)) retlist)))
	    (push (srt-anki--transform-entry (match-string 0)) retlist))
      retlist)))

(defun srt-anki-change-entries (fun)
  "iterate over subtitles and apply fun to each element, fun
should get sub structure as an argument and return sub structure as a argument"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp srt-anki--regexp nil t)
      (let* ((mb (match-beginning 0))
             (me (match-end 0))
             (sub (srt-anki--transform-entry (match-string 0)))
             (retsub (funcall fun sub)))
        (when retsub
          (delete-region mb me)
          (insert (srt-anki--retransform-entry retsub ) "\n"))))))
        
(defun srt-anki-add-100msec ()
  "add 100 msec to all subtitles"
  (interactive)
  (srt-anki-change-entries #'(lambda (el)
                              (setf (sub-start-time el) (+ 0.1 (sub-start-time el)))
                              (setf (sub-end-time el) (+ 0.1 (sub-end-time el)))
                              el)))

(defun srt-anki-framerate-23-to-25 ()
  "convert framerate from 23.976 to 25 FPS"
  (interactive)
  (srt-anki-change-entries #'(lambda (el)
                               (setf (sub-start-time el) (/ (* 25.0 (sub-start-time el)) 23.976))
                               (setf (sub-end-time el) (/ (* 25.0 (sub-end-time el)) 23.976))
                               el)))

(defun srt-anki-framerate-25-to-23 ()
  "convert framerate from 25 to 23.976 FPS"
  (interactive)
  (srt-anki-change-entries #'(lambda (el)
                               (setf (sub-start-time el) (/ (* 23.976 (sub-start-time el)) 25.0))
                               (setf (sub-end-time el) (/ (* 23.976 (sub-end-time el)) 25.0))
                               el)))

             

(defun srt-anki-prepare-for-hunalign (srtname)
  "parses the srtname and converts txt files and remove newlines"
  (interactive "f")
  (let (retlist txtlist)
    (with-temp-buffer
      (insert-file-contents srtname)
      (goto-char (point-min))
      (while (search-forward-regexp srt-anki--regexp nil t)
	    (push (sub-text (srt-anki--transform-entry (match-string 0))) txtlist)))
    (setq txtlist (mapcar #'(lambda (element)
                             (setq element (replace-regexp-in-string "\n" " <n> " element))
                             (replace-regexp-in-string "\\[[[:print:]]+\\]" "" element))
                          (nreverse txtlist)))
    (find-file (concat (file-name-sans-extension srtname) "_preproc.txt"))
    (erase-buffer)
    (dolist (el txtlist)
      (insert el "\n"))
    (save-buffer)))

;;(setq yy (srt-anki--parse-file "/home/rrybanie/.emacs.d/ankitools/blee.srt"))


(defun srt-anki--get-sub-from-time-txt (srtdb time)
  "get subtitle from srtdb that is visible on time time"
  (sub-text (srt-anki--get-sub-from-time srtdb time)))

(defun srt-anki--get-sub-from-time (srtdb time)
  "get subtitle from srtdb that is visible on time time"
  (cl-some (lambda (elt)
	     (when (and (>= time (sub-start-time elt))
			(<= time (sub-end-time elt)))
	       elt)) srtdb))


(defun srt-anki-produce-from-file (fname)
  "parse MPV resulst file and produce org file"
  (with-temp-buffer
    (insert-file-contents fname)
    (goto-char (point-min))
    (when (re-search-forward "Subs.*?'\\([[:print:]]+.srt\\)'" nil t)
      (let ((srtdb (srt-anki--parse-file (match-string 1))))
	(while (re-search-forward "bleeek \\([0-9.]+\\)" nil t)
	  (message "%s" (srt-anki--get-sub-from-time-txt srtdb (string-to-number (match-string 1)))))))))


(defun srt--anki-gen-audio (mediafile srtstart srtend)
  "generate audio clip from the mediafile with start and stop"
  (format "%s_%d_%d.mp3" mediafile srtstart srtend))

(defun srt--anki-gen-img (mediafile srtstart srtend)
  "generate screenshot from the mediafile with start and stop"
  (format "%s_%d_%d.png" mediafile srtstart srtend))


(defun srt-anki-produce-from-file (fname)
  "parse MPV resulst file and produce org file"
  (interactive "fEnter MPV output name: ") 
  (with-temp-buffer
    (insert-file-contents fname)
    (goto-char (point-min))
    (when (re-search-forward "Playing: \\([[:print:]]+\\)" nil t)
      (let ((mediafile (match-string 1)))
	    (when (re-search-forward "Subs.*?'\\([[:print:]]+.srt\\)'" nil t)
	      (let* ((subfile (match-string 1))
		         (srtdb (srt-anki--parse-file subfile)))
            (with-current-buffer (get-buffer-create "srt-anki-buf.org")
              (erase-buffer)
              (insert "#+SRT_MODE: 1\n")
              (insert "#+SRT_PREFIX: srtprefix\n")
              (insert "#+SRT_ADD_PRE: 0.0\n");
              (insert "#+SRT_ADD_POST: 0.0\n");
              (insert (format "#+SRT_MEDIAFILE: %s\n" mediafile));
              (insert (format "#+SRT_SUBFILE: %s\n" subfile));
              (insert "#+MACRO: BR @@html:<br>@@\n\n"))
	        (while (re-search-forward "bleeek \\([0-9.]+\\)" nil t)
	          (let* ((elt (srt-anki--get-sub-from-time srtdb (string-to-number (match-string 1))))
		             (srtstring (sub-text elt))
		             (srtstart (sub-start-time elt))
		             (srtend (sub-end-time elt)))
		        ;; remove newlines and change to <br>
		        (setq srtstring (replace-regexp-in-string "\n" "{{{BR}}}" srtstring))
		        (setq srtstring (replace-regexp-in-string "<b>[ ]*" "*" srtstring))
		        (setq srtstring (replace-regexp-in-string "</b>[ ]*" "*" srtstring))
		        (setq srtstring (replace-regexp-in-string "<i>[ ]*" "/" srtstring))
		        (setq srtstring (replace-regexp-in-string "</i>[ ]*" "/" srtstring))
		        (with-current-buffer (get-buffer-create "srt-anki-buf.org")
		          ;;(org-mode)
                  
		          (insert (format "* %s\n" srtstring))
		          (org-id-get-create)
		          (org-set-property "SUBSTART" (number-to-string srtstart))
		          (org-set-property "SUBEND" (number-to-string srtend)))))))))))

(defun srt-anki-org-from-srt (fname)
  "parse srt file and produce org file"
  (interactive "fEnter SRT file name: ")
  (let ((srtdb (nreverse (srt-anki--parse-file fname))))
    (when srtdb
      (with-current-buffer (get-buffer-create "srt-anki-buf.org")
        (erase-buffer)
        (insert "#+SRT_MODE: 1\n")
        (insert "#+SRT_PREFIX: srtprefix\n")
        (insert "#+SRT_ADD_PRE: 0.0\n");
        (insert "#+SRT_ADD_POST: 0.0\n");
        (insert (format "#+SRT_MEDIAFILE: %s\n" mediafile));
        (insert (format "#+SRT_SUBFILE: %s\n" fname));
        (insert "#+MACRO: BR @@html:<br>@@\n\n");
        (dolist (elt srtdb)
          (let ((srtstring (sub-text elt))
                (srtstart (sub-start-time elt))
                (srtend (sub-end-time elt)))
		      (setq srtstring (replace-regexp-in-string "<b>[ ]*" "*" srtstring))
		      (setq srtstring (replace-regexp-in-string "</b>[ ]*" "*" srtstring))
		      (setq srtstring (replace-regexp-in-string "<i>[ ]*" "/" srtstring))
		      (setq srtstring (replace-regexp-in-string "</i>[ ]*" "/" srtstring))
              (setq srtstring (replace-regexp-in-string "\n" " {{{BR}}} " srtstring))
              (insert (format "* %s\n" srtstring))
		      (org-id-get-create)
		      (org-set-property "SUBSTART" (number-to-string srtstart))
		      (org-set-property "SUBEND" (number-to-string srtend))))))))

(defun srt-anki-orglify-sentence (txt)
  "replcae html tags to org compatible"
  (setq txt (replace-regexp-in-string "<b>[ ]*" "*" txt))
  (setq txt (replace-regexp-in-string "</b>[ ]*" "*" txt))
  (setq txt (replace-regexp-in-string "<i>[ ]*" "/" txt))
  (setq txt (replace-regexp-in-string "</i>[ ]*" "/" txt))
  (setq txt (replace-regexp-in-string "**" "" txt))
  (setq txt (replace-regexp-in-string "//" "" txt))
  (setq txt (replace-regexp-in-string "<n>" " {{{BR}}} " txt))
  (setq txt (replace-regexp-in-string "\n" " {{{BR}}} " txt))
  (setq txt (replace-regexp-in-string "  " " " txt)))

(defun srt-anki-load-hunalign-sentences (infile)
  "load sentences from file and save as a list"
  (with-temp-buffer
    (insert-file-contents infile)
    (goto-char (point-min))
    (let ((senta nil)
          (sentb nil)
          (i 0))
      (while (re-search-forward "^\\(.*?\\)	\\(.*?\\)	.*" nil t)
        (push (cons (srt-anki-orglify-sentence (match-string 1)) (srt-anki-orglify-sentence (match-string 2))) senta))
      (setq senta (nreverse senta))
      senta)))

(defun srt-anki-add-translations (fname)
  "put translations from the hunaligned file"
  (interactive "fEnter hunaligned file name: ")
  ;; load sentences
  (let ((sentences (srt-anki-load-hunalign-sentences fname)))
    (dolist (elt sentences)
      ;;(search-forward (car elt) nil nil)
      (search-forward (car elt) nil t)
      (rr-org-next-h 1)
      (insert "** " (cdr elt) "\n"))))

;;;;;;;;;;;;;;
;; org mode functions
;;;;;;;;;;;;;;

(defun srt-anki--parse-keywords ()
  "parse keywords in current buffer and return as a plist"
  (save-excursion
    (let ((retlist nil))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([A-Za-z_-]+\\): *\\([[:print:]]+\\)" nil t)
        (setq retlist (plist-put retlist (intern (match-string 1)) (match-string 2))))
      retlist)))
                          
    
(defun srt-anki--play (fname start end)
  (if (file-exists-p fname)
      (process-lines "/usr/bin/mpv" "--geometry=640"  fname (format "--start=%f" start) (format "--ab-loop-a=%f" start) (format "--ab-loop-b=%f" end))
    (message "File %s does not exist" fname)))

(defun srt-anki-play ()
  "play video based on srt entry"
  (interactive)
  (let* ((retlist (srt-anki--parse-keywords))
         (fname (plist-get retlist 'SRT_MEDIAFILE))
         (add-pre (string-to-number (plist-get retlist 'SRT_ADD_PRE)))
         (add-post (string-to-number (plist-get retlist 'SRT_ADD_POST)))
	     (start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
         (start (- start add-pre))
	     (end (string-to-number (cdar (org-entry-properties nil "SUBEND"))))
         (end (+ end add-post)))

    (srt-anki--play fname start end)
    (message "play %f %f\n" start end)))

(defun srt--anki-gen-audio (infile outfile start end)
  "generate audio clip from the infile with start and stop and save in outfile"
  (when (buffer-file-name)
    (unless (file-directory-p (rr-anki--media-dir (buffer-file-name)))
      (make-directory (rr-anki--media-dir (buffer-file-name))))
    (when (process-lines "/usr/bin/mpv" "--vid=no" infile (format "--start=%f" start) (format "--end=%f" end)
			 "-o" (format "%s/%s" (rr-anki--media-dir (buffer-file-name)) outfile))
      outfile)))

(defun srt--anki-gen-screenshot (infile outfile start end)
  "generate audio clip from the infile with start and stop and save in outfile"
  (when (buffer-file-name)
    (unless (file-directory-p (rr-anki--media-dir (buffer-file-name)))
      (make-directory (rr-anki--media-dir (buffer-file-name))))
    ;; (when (process-lines
    ;;        "/usr/bin/mpv" "--aid=no" infile
    ;;        (format "--start=%f" (/ (+ start end) 2))
    ;;        (format "--length=%f" 0.06)
    ;;        "--of=singlejpeg"
    ;;        "--sub=no"
    ;;        "--vf=scale=250:120"
	;; 	   "-o" (format "%s/%s" (rr-anki--media-dir (buffer-file-name)) outfile))

      (when (process-lines
             "/usr/bin/ffmpeg" "-y"
             "-ss" (number-to-string (/ (+ start end) 2)) ;; -ss postion matters
             "-i" infile
             "-vframes" "1"
             "-vf" "scale=320:-1"
             "-q:v" "2"
		     (format "%s/%s" (rr-anki--media-dir (buffer-file-name)) outfile))
      outfile)))

(defun rr-generate-audio-srt ()
  "generate audio from for current heading, if not already generated,
with universal argument always regenerate"
  (interactive)
  (unless (buffer-file-name)
    (when (y-or-n-p ("You have to save the buffer first. Save it?"))
      (save-buffer)))
  (when (buffer-file-name)
    (let ((infile rr-anki-input-media)
          ;;(infile (cdar (org-entry-properties nil "VIDFILE")))
	      (outfile (concat  (org-id-get-create) ".mp3"))
	      (start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
	      (end (string-to-number (cdar (org-entry-properties nil "SUBEND")))))
      (when (or current-prefix-arg
	            (not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" outfile))))
        (srt--anki-gen-audio infile outfile start end)))))

(defun rr-generate-screenshot-srt ()
  "generate audio from for current heading, if not already generated,
with universal argument always regenerate"
  (interactive)
  (unless (buffer-file-name)
    (when (y-or-n-p ("You have to save the buffer first. Save it?"))
      (save-buffer)))
  (when (buffer-file-name)
    (let ((infile rr-anki-input-media)
	      (outfile (concat  (org-id-get-create) ".jpg"))
	      (start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
	      (end (string-to-number (cdar (org-entry-properties nil "SUBEND")))))
      (when (or current-prefix-arg
		        (not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" outfile))))
	    (srt--anki-gen-screenshot infile outfile start end)))))

(defun rr-srt-shift-subtitle (time)
  "shift subtitles by time"
  (interactive)
    (let ((start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
	      (end (string-to-number (cdar (org-entry-properties nil "SUBEND")))))
      (org-set-property "SUBSTART" (number-to-string (+ start time)))
      (org-set-property "SUBEND" (number-to-string (+ end time)))))


(defun rr-org-srt--org-to-struct ()
    "return srt-struct from org heading at point"
  (make-sub :start-time (string-to-number (cdar (org-entry-properties nil "SUBSTART")))
            :end-time (string-to-number (cdar (org-entry-properties nil "SUBEND")))))

(defun rr-org-srt--struct-to-org-properites (sub)
  "update heading at point from srt-struct"
  (org-set-property "SUBSTART" (number-to-string (sub-start-time sub)))
  (org-set-property "SUBEND" (number-to-string (sub-end-time sub))))

;; iterator
(defun rr-org-srt-change-entries (fun)
  "iterate over subtitles and apply fun to each element, fun
should get sub structure as an argument and return sub structure as a argument"
  (save-excursion
    (goto-char (point-min))
    (while (rr-org-next-h-in-subtree 1)
      (let ((sub (rr-org-srt--org-to-struct)))
        (rr-org-srt--struct-to-org-properites (funcall fun sub))))))


(defun rr-org-srt-add-100msec ()
  "add 100 msec to all subtitles"
  (interactive)
  (rr-org-srt-change-entries #'(lambda (el)
                               (setf (sub-start-time el) (+ 0.1 (sub-start-time el)))
                               (setf (sub-end-time el) (+ 0.1 (sub-end-time el)))
                               el)))

(defun rr-org-srt-framerate-23-to-25 ()
  "convert framerate from 23.976 to 25 FPS"
  (interactive)
  (rr-org-srt-change-entries #'(lambda (el)
                                 (setf (sub-start-time el) (/ (* 25.0 (sub-start-time el)) 23.976))
                                 (setf (sub-end-time el) (/ (* 25.0 (sub-end-time el)) 23.976))
                                 el)))

(defun rr-org-srt-framerate-25-to-23 ()
  "convert framerate from 25 to 23.976 FPS"
  (interactive)
  (rr-org-srt-change-entries #'(lambda (el)
                                 (setf (sub-start-time el) (/ (* 23.976 (sub-start-time el)) 25.0))
                                 (setf (sub-end-time el) (/ (* 23.976 (sub-end-time el)) 25.0))
                                 el)))




(defun rr-srt-genarate-media-in-buffer ()
  "generate screenshots and audionfor for the whole buffer,
   with prefix argument update even if exist"
  (interactive)
  (if (not (buffer-file-name))
      (message "Save buffer first!")
    (save-excursion
      (let* ((cnt-hdr 0)
	         (sub-cnt-hdr 0)
	         (sub-cnt 0)
             (retlist (srt-anki--parse-keywords))
             (mediafile (plist-get retlist 'SRT_MEDIAFILE))
             (srtmode (plist-get retlist 'SRT_MODE))
             (prefix  (plist-get retlist 'SRT_PREFIX))
             (add-pre (string-to-number (plist-get retlist 'SRT_ADD_PRE)))
             (add-post (string-to-number (plist-get retlist 'SRT_ADD_POST))))
        (if (not srtmode)
            (message "input file is not in srtmode")
          ;;else
	      (goto-char (point-min))
	      (while (rr-org-next-h-in-subtree 1)
	        (incf cnt-hdr)
	        (when (cdar (org-entry-properties nil "SUBSTART"))
	          (incf sub-cnt-hdr)))

	      (message "Headers: %d SRT Headers: %d\n" cnt-hdr sub-cnt-hdr)
	      (goto-char (point-min))
	      (while (rr-org-next-h-in-subtree 1)
	        (when (cdar (org-entry-properties nil "SUBSTART"))
	          (message "Generating SRT Media %d/%d" (incf sub-cnt) sub-cnt-hdr)
              (let ((outfile (concat  (if prefix (concat prefix "-") nil) (org-id-get-create)))
	                (start (- (string-to-number (cdar (org-entry-properties nil "SUBSTART"))) add-pre))
	                (end (+ (string-to-number (cdar (org-entry-properties nil "SUBEND"))) add-post)))
                 (when (or current-prefix-arg
		                   (not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" (concat outfile ".jpg")))))
	               (srt--anki-gen-screenshot mediafile (concat outfile ".jpg") start end))
                   
                 (when (or current-prefix-arg
		                   (not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" (concat outfile ".mp3")))))
                   (srt--anki-gen-audio mediafile (concat outfile ".mp3") start end)))))
          (message "Done!")
          )))))

;; (setq teststring "blek
;; newline")

;; (re)

;; (replace-regexp-in-string "\n" "<br>" teststring)
      
