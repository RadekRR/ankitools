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



(setq yy (srt-anki--parse-file "/home/rrybanie/.emacs.d/ankitools/blee.srt"))


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
  (with-temp-buffer
    (insert-file-contents fname)
    (goto-char (point-min))
    (with-current-buffer (get-buffer-create "srt-anki-buf.org")
      (erase-buffer))
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
		(message "%s" srtstring)
		(with-current-buffer (get-buffer-create "srt-anki-buf.org")
		  (insert (format "* %s\n" srtstring))
		  (org-set-property "SUBFILE" subfile)
		  (org-set-property "SUBSTART" (number-to-string srtstart))
		  (org-set-property "SUBEND" (number-to-string srtend))
		  (org-set-property "SUBMP3" (srt--anki-gen-audio mediafile srtstart srtend))
		  (org-set-property "SUBIMG" (srt--anki-gen-img mediafile srtstart srtend)))))))))))

    
    
(setq lista nil)
(setq listb '(a b c))


