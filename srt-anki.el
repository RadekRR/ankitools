(setq srt-anki--regexp "^[0-9]+
\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\) --> \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)
\\([[:print:]
]+?\\)

")

(defun srt-anki--transform-entry (subtxt)
  "return property list from one entry of srt subtitle"
  (with-temp-buffer
    (insert subtxt)
    (goto-char (point-min))
    (when (search-forward-regexp srt-anki--regexp nil t) ;; or string-match
      (let ((fhrs (string-to-number (match-string 1)))
	    (fmin (string-to-number (match-string 2)))
	    (fsec (string-to-number (match-string 3)))
	    (fmec (string-to-number (match-string 4)))

	    (thrs (string-to-number (match-string 5)))
	    (tmin (string-to-number (match-string 6)))
	    (tsec (string-to-number (match-string 7)))
	    (tmec (string-to-number (match-string 8)))

	    (subt (match-string 9))
	    (retlist (list 'tstart nil 'tend nil 'substr nil)))
	(setq retlist (plist-put retlist 'tstart (+ (* fhrs 60 60) (* fmin 60) fsec (/ fmec 1000.0))))
	(setq retlist (plist-put retlist 'tend (+ (* thrs 60 60) (* tmin 60) tsec (/ tmec 1000.0))))
	(setq retlist (plist-put retlist 'substr subt))))))



(setq testtxt "1194
02:10:10,544 --> 02:10:16,717
Tym, który z otchłani mocy powrócił,
jest twój stary mistrz.

")

(defun srt-anki--parse-file (srtname)
  "parses the srtname file and returns list of plists of all entries"
  (with-temp-buffer
    (insert-file-contents srtname)
    (goto-char (point-min))
    (let (retlist)
      (while (search-forward-regexp srt-anki--regexp nil t)
	;;(setq retlist (append (srt-anki--transform-entry (match-string 0)) retlist)))
	(push (srt-anki--transform-entry (match-string 0)) retlist))
      retlist)))



;;(setq yy (srt-anki--parse-file "/home/rrybanie/.emacs.d/ankitools/blee.srt"))


(defun srt-anki--get-sub-from-time-txt (srtdb time)
  "get subtitle from srtdb that is visible on time time"
  (plist-get (srt-anki--get-sub-from-time srtdb time) 'substr ))

(defun srt-anki--get-sub-from-time (srtdb time)
  "get subtitle from srtdb that is visible on time time"
  (cl-some (lambda (elt)
	     (when (and (>= time (plist-get elt 'tstart))
			(<= time (plist-get elt 'tend)))
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
    (with-current-buffer (get-buffer-create "srt-anki-buf.org")
      (erase-buffer)
      (insert "#+MACRO: BR @@html:<br>@@\n\n"))
    (when (re-search-forward "Playing: \\([[:print:]]+\\)" nil t)
      (let ((mediafile (match-string 1)))
	(when (re-search-forward "Subs.*?'\\([[:print:]]+.srt\\)'" nil t)
	  (let* ((subfile (match-string 1))
		 (srtdb (srt-anki--parse-file subfile)))
	    (while (re-search-forward "bleeek \\([0-9.]+\\)" nil t)
	      (let* ((elt (srt-anki--get-sub-from-time srtdb (string-to-number (match-string 1))))
		     (srtstring (plist-get elt 'substr))
		     (srtstart (plist-get elt 'tstart))
		     (srtend (plist-get elt 'tend)))
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
		  (org-set-property "SUBFILE" subfile)
		  (org-set-property "VIDFILE" mediafile)
		  (org-set-property "SUBSTART" (number-to-string srtstart))
		  (org-set-property "SUBEND" (number-to-string srtend)))))))))))

    
(defun srt-anki--play (fname start end)
  (if (file-exists-p fname)
      (process-lines "/usr/bin/mpv" "--geometry=640"  fname (format "--start=%f" start) (format "--ab-loop-a=%f" start) (format "--ab-loop-b=%f" end))
    (message "File %s does not exist" fname)))

(defun srt-anki-play ()
  "play video based on srt entry"
  (interactive)
  (let ((fname (cdar (org-entry-properties nil "VIDFILE")))
	(start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
	(end (string-to-number (cdar (org-entry-properties nil "SUBEND")))))
    (srt-anki--play fname start end)))

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
    (when (process-lines "/usr/bin/mpv" "--aid=no" infile (format "--start=%f" start) (format "--length=%f" 0.03) "--of=singlejpeg" "--sub=no" "--vf=scale=250:120"
			 "-o" (format "%s/%s" (rr-anki--media-dir (buffer-file-name)) outfile))
      outfile)))

(defun rr-generate-audio-srt ()
  "generate audio from for current heading, if not already generated,
with universal argument always regenerate"
  (interactive)
  (unless (buffer-file-name)
    (when (y-or-n-p ("You have to save the buffer first. Save it?"))
      (save-buffer)))
  (when (buffer-file-name)
    (let ((infile (cdar (org-entry-properties nil "VIDFILE")))
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
    (let ((infile (cdar (org-entry-properties nil "VIDFILE")))
	  (outfile (concat  (org-id-get-create) ".jpg"))
	  (start (string-to-number (cdar (org-entry-properties nil "SUBSTART"))))
	  (end (string-to-number (cdar (org-entry-properties nil "SUBEND")))))
      (when (or current-prefix-arg
		(not (file-exists-p (concat (rr-anki--media-dir (buffer-file-name)) "/" outfile))))
	(srt--anki-gen-screenshot infile outfile start end)))))


;; (setq teststring "blek
;; newline")

;; (re)

;; (replace-regexp-in-string "\n" "<br>" teststring)
      
