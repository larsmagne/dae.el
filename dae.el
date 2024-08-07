;;; dae.el --- Digital Audio Extraction
;; Copyright (C) 2011 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; dae.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dae.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dae.el; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This library provides an interface for ripping, scanning and
;; querying/editing cddb/musicbrainz entries in parallell.  The
;; parallellism requires that you have several CD ROM players.

;;; Code:

(require 'cddb)
(require 'scan)
(require 'musicbrainz)

(defvar dae-cdrom "/dev/addonics%d"
  "CD-ROM device.")

(defvar dae-directory "/stage/"
  "Base directory where extracted files will be stored.")

(defvar dae-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" 'dae-read-audio-cd-1)
    (define-key map "2" 'dae-read-audio-cd-2)
    (define-key map "3" 'dae-read-audio-cd-3)
    (define-key map "4" 'dae-read-audio-cd-4)
    (define-key map "9" 'dae-read-audio-audio-sr0)
    map))

(define-minor-mode dae-mode
  "Minor mode for Digital Audio Extraction."
  :lighter " DAE" :keymap dae-mode-map nil)

(defun dae-read-audio-cd-1 (&optional paranoid)
  (interactive "P")
  (dae-read-numbered-cdrom 1 paranoid))

(defun dae-read-audio-cd-2 (&optional paranoid)
  (interactive "P")
  (dae-read-numbered-cdrom 2 paranoid))

(defun dae-read-audio-cd-3 ()
  (interactive)
  (dae-read-numbered-cdrom 3))

(defun dae-read-audio-cd-4 ()
  (interactive)
  (dae-read-numbered-cdrom 4))

(defun dae-read-audio-sr0 ()
  (interactive)
  (dae-read-audio-cd "/dev/sr0"))

(defun dae-read-numbered-cdrom (number &optional paranoid)
  (dae-read-audio-cd (format dae-cdrom number) paranoid))

(defun dae-clean-title (string)
  (string-trim (replace-regexp-in-string
		" / " "/" (replace-regexp-in-string "\\\\'" "'" string))))

(defun dae-possibly-get-titles (cdrom)
  ;; icedax writes a cddb file automatically, and we pick that up.
  ;; But if it's a Various album, the group info isn't in that cddb
  ;; file, so we have to parse this alternative output.
  (with-temp-buffer
    (call-process "icedax" nil t nil
		  "-J" "-g" "-v" "toc,titles" "-B" "-D" cdrom)
    (goto-char (point-min))
    (let ((titles nil))
      (while (re-search-forward "title '\\(.*\\)' from '\\(.*\\)'$"
				nil t)
	(let ((track (match-string 1))
	      (group (match-string 2)))
	  (push (concat (dae-clean-title group) " / "
			(dae-clean-title track))
		titles)))
      (and titles
	   (list (cons 'tracks (nreverse titles)))))))

(defun dae-read-audio-cd (cdrom &optional paranoid)
  (let* ((titles (dae-possibly-get-titles cdrom))
	 (data (dae-anonymous-read-audio-cd cdrom paranoid))
	 (dir (car data))
	 (frames (nth 1 data))
	 id cat max id-file result confirmed
	 file)
    (setq id (cdr (assq 'id frames))
	  file "/tmp/id"
	  result (and nil (cddb-query frames)))
    (setq cat (car result)
	  result (cdr result))
    (when (file-exists-p file)
      (delete-file file))
    (when result
      (with-current-buffer result
	(write-region (point-min) (point-max) file
		      nil 'silent))
      (unless (setq confirmed
		    (y-or-n-p (format "The cd is %s/%s? "
				      (cddb-parse file 'artist)
				      (cddb-parse file 'title))))
	(setq result nil)))
    ;; No result from freedb -- query MusicBrainz.
    (when (and nil
	       (not result))
      (let ((mb-id (cdr (assq 'id (nth 3 data)))))
	(message "Querying MusicBrainz (%s)..." mb-id)
	(let ((xml (musicbrainz-query mb-id)))
	  (when xml
	    (when (setq result (musicbrainz-to-cddb xml))
	      (cddb-write-file file result))))))
    ;; If we didn't get a result from the data bases, we look for the
    ;; audio.cddb file.  If the CD has the track names stored on disc
    ;; ("CD TEXT"), we'll find them there.
    (let ((cddb (expand-file-name "audio.cddb" dir)))
      (message "Looking for CD TXT...")
      (when (and (null result)
		 (file-exists-p cddb))
	(setq result (cddb-parse cddb))
	(cddb-write-file file result)))
    (if (and (file-exists-p file)
	     (or confirmed
		 (y-or-n-p (format "The cd is %s/%s? "
				   (cddb-parse file 'artist)
				   (cddb-parse file 'title)))))
	(cddb-edit (cddb-merge (or titles (cddb-parse file)) frames)
		   cat)
      (let* ((artist (read-string "Artist: "))
	     (alist (cddb-grep artist))
	     (album (completing-read "Album: " alist))
	     (did (cdr (assoc album alist)))
	     names)
	(if did
	    (cddb-edit
	     (cddb-merge (cddb-parse (expand-file-name did cddb-directory))
			 frames)
	     cat)
	  (cddb-edit
	   `((artist . ,artist)
	     (title . ,album)
	     ,@frames)))))
    (setq cddb-submit-hook
	  `(lambda ()
	     (let ((file (concat ,dir "id"))
		   (process ,(nth 2 data))
		   (toc ',(nth 3 data)))
	       (write-region (point-min) (point-max) file)
	       (dae-ensure-directory
		(expand-file-name "new-cdda" dae-directory))
	       (write-region (point-min) (point-max)
			     (concat dae-directory "new-cdda/"
				     ,id))
	       (when (and nil
			  toc)
		 (musicbrainz-possibly-submit toc (cddb-parse file)))
	       (when (or (not process)
			 (not (memq (process-status process)
				    '(open run))))
		 (dae-rename-raw ,dir)))))))

(defun dae-anonymous-read-audio-cd (cdrom &optional paranoid)
  "Read an anonymous audio CD."
  (interactive)
  (let (id frames dir process toc)
    (setq frames (cddb-toc cdrom)
	  toc (musicbrainz-toc cdrom)
	  id (cdr (assq 'id frames)))
    (setq dir (concat dae-directory "anonymous/" id "/"))
    (dae-ensure-directory dir)
    (setq process
	  (if paranoid
	      (dae-start-cdda dir cdrom)
	    (dae-start-paranoid dir cdrom)))
    (scan-sleeve dir)
    (list dir frames process toc)))

(defun dae-start-cdda (dir cdrom)
  (let* ((default-directory dir)
	 (start-time (float-time (current-time)))
	 (process
	  (start-process
	   "*sample*" (get-buffer-create " *cdda2wav*")
	   "xterm"
	   "-e"
	   "icedax"
	   ;;"-paranoia"
	   "-v" "toc,titles"
	   "-B" (concat "-D" cdrom))))
    (set-process-sentinel
     process
     `(lambda (process change)
	(when (file-exists-p (expand-file-name "id" ,dir))
	  (dae-report-time ,start-time ,dir)
	  (dae-rename-raw ,dir))
	(dae-eject ,cdrom)))
    process))

(defun dae-start-paranoid (dir cdrom)
  (let* ((default-directory dir)
	 (start-time (float-time (current-time)))
	 process)
    (call-process "icedax" nil (get-buffer-create " *cdda2wav*") nil
		  "-v" "toc,titles"
		  "-J" (concat "-D" cdrom))
    (setq process
	  (start-process
	   "*sample*" (get-buffer-create " *cdda2wav*")
	   "xterm" "-e" "cdparanoia"
	   "-Z"
	   "-B" "-d" cdrom))
    (set-process-sentinel
     process
     `(lambda (process change)
	(when (file-exists-p (expand-file-name "id" ,dir))
	  (dae-report-time ,start-time ,dir)
	  (dae-rename-raw ,dir))
	(dae-eject ,cdrom)))
    process))

(defun dae-report-time (start-time dir)
  (let ((size 0)
	(elapsed (- (float-time (current-time))
		    start-time)))
    (dolist (file (directory-files dir t "\\.wav$"))
      (cl-incf size (nth 7 (file-attributes file))))
    (message "Extracted in %02d:%02d (%.1fx speedup)"
	     (truncate (/ elapsed 60))
	     (mod elapsed 60)
	     (/ size (* 44100 2 2) elapsed))))

(defun dae-rename-raw (dir)
  ;; If we're using cdparanoia, rename the files first.
  (dolist (file (directory-files dir t "track[0-9]+.cdda.wav$"))
    (rename-file file (replace-regexp-in-string
		       "track\\([0-9]+\\).cdda.wav$"
		       "audio_\\1.wav"
		       file)))
  (when (file-exists-p (expand-file-name "id" dir))
    (let* ((default-directory dir)
	   (id (concat dir "id"))
	   (tracks (cddb-parse id 'tracks))
	   (i 1))
      (unless (string= (cddb-parse id 'title) "")
	(while tracks
	  (when (file-exists-p (format "audio_%02d.wav" i))
	    (rename-file
	     (format "audio_%02d.wav" i)
	     (format "%02d-%s.wav" i
		     (dae-quote (car tracks)))
	     t))
	  (when (file-exists-p (format "audio_%02d.inf" i))
	    (delete-file (format "audio_%02d.inf" i)))
	  (cl-incf i)
	  (pop tracks))
	(dolist (file '("audio.cddb" "audio.cdindex"))
	  (when (file-exists-p (expand-file-name file dir))
	    (delete-file (expand-file-name file))))
	(let ((target-dir 
	       (expand-file-name (concat (dae-quote (cddb-parse id 'artist))
					 "/"
					 (dae-quote (cddb-parse id 'title)))
				 (concat dae-directory "processing"))))
	  (dae-ensure-directory target-dir)
	  (dolist (file (directory-files dir t))
	    (when (file-regular-p file)
	      (rename-file file (expand-file-name (file-name-nondirectory file)
						  target-dir)
			   t)))
	  (delete-directory dir)
	  (with-temp-file (expand-file-name "stats" target-dir)
	    (setq id (expand-file-name "id" target-dir))
	    (insert (format
		     "Artist: %s\nTitle: %s\nSource: cd\nCDDB: %s\nYear: %s\nTime: %s\n\n"
		     (cddb-parse id 'artist)
		     (cddb-parse id 'title)
		     (cddb-parse id 'id)
		     (or (cddb-parse id 'year) "")
		     (format-time-string "%Y%m%dT%H%M%S")))))))))

(defun dae-ensure-directory (dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun dae-quote (name)
  (while (string-match "/" name)
    (setq name (replace-match "-" nil t name)))
  name)

(defun dae-eject (cdrom)
  "Eject a CD."
  (start-process "*sh*" nil "eject" cdrom))

(provide 'dae)

;;; dae.el ends here
