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

(require 'cl)
(require 'cddb)
(require 'scan)

(defvar dae-cdrom "/dev/addonics%d"
  "CD-ROM device.)

(defvar dae-directory "/stage/"
  "Base directory where extracted files will be stored.")

(defvar dae-this-cd-process nil)

(defvar gnus-dead-summary-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "1" 'dae-read-audio-cd-1)
    (define-key map "2" 'dae-read-audio-cd-2)
    (define-key map "3" 'dae-read-audio-cd-3)))

(define-minor-mode dae-mode
  "Minor mode for Digital Audio Extraction."
  :lighter " DAE" :keymap dae-mode-map)

(defun dae-read-audio-cd-1 ()
  (interactive)
  (dae-read-numbered-cdrom 1))

(defun dae-read-audio-cd-2 ()
  (interactive)
  (dae-read-numbered-cdrom 2))

(defun dae-read-audio-cd-3 ()
  (interactive)
  (dae-read-numbered-cdrom 3))

(defun dae-read-numbered-cdrom (number)
  (dae-read-audio-cd (format dae-cdrom number)))

(defun dae-read-audio-cd (cdrom)
  (let* ((data (dae-anonymous-read-audio-cd cdrom))
	 (dir (car data))
	 (frames (cdr data))
	 id cat file max id-file result)
    (setq id (cdr (assq 'id frames)))
    (setq result (cddb-query frames)
	  file (car result))
    ;; No result from freedb -- query MusicBrainz.  cdda2wav puts the
    ;; MusicBrainz CD id into the "audio.inf" file, so look for it
    ;; there.
    (unless result
      (with-temp-buffer
	(insert-file-contents (expand-file-name "audio.inf" dir))
	(goto-char (point-min))
	(when (re-search-forward "CDINDEX_DISCID=.*'\\([^']+\\)'" nil t)
	  (let ((xml (musicbrainz-query (match-string 1))))
	    (when xml
	      (setq result (musicbrainz-to-cddb xml)))))))
    ;; If we didn't get a result from the data bases, we look for the
    ;; audio.cddb file.  If the CD has the track names stored on disc
    ;; ("CD TEXT"), we'll find them there.
    (let ((cddb (expand-file-name "audio.cddb" dir)))
      (when (and (null result)
		 (file-exists-p cddb))
	(setq result (list cddb (cddb-parse cddb))
	      file cddb)))
    (if (and file
	     (y-or-n-p (format "The cd is %s? " (cddb-parse file 'title))))
	(progn
	  (cddb-edit (cddb-merge (cddb-parse file) frames)
		     (cdr result))
	  (when (and nil (equal (system-name) "potato.gnus.org"))
	    (cddb-submit)))
      (let* ((artist (read-string "Artist: "))
	     (alist (dae-grep-cddb artist))
	     (album (completing-read "Album: " alist))
	     (did (cdr (assoc album alist)))
	     names)
	(if did
	    (cddb-edit
	     (cddb-merge (cddb-parse (concat "/mnt/cddb/" did)) frames)
	     (cdr result))
	  (cddb-edit
	   `((artist . ,artist)
	     (title . ,album)
	     ,@frames)))))
    (setq cddb-submit-hook
	  `(lambda ()
	     (let ((file (concat ,dir "id"))
		   (process ,(nth 2 data)))
	       (write-region (point-min) (point-max) file)
	       (write-region (point-min) (point-max)
			     (concat dae-directory "data/new-cdda/"
				     ,id))
	       (when (or (not process)
			 (not (memq (process-status process)
				    '(open run))))
		 (dae-rename-raw ,dir)))))))

(defun dae-anonymous-read-audio-cd (cdrom)
  "Read an anonymous audio CD."
  (interactive)
  (let (id frames dir process)
    (setq frames (dae-read-frames)
	  id (cdr (assq 'id frames)))
    (setq dir (concat dae-directory "anonymous/" id "/"))
    (dae-ensure-directory dir)
    (setq process (dae-start-cdda dir cdrom))
    (scan-sleeve dir)
    (cons file frames process)))

(defun dae-start-cdda (dir cdrom)
  (let* ((default-directory dir)
	 (process
	  (start-process
	   "*sample*" (get-buffer-create " *cdda2wav*")
	   "xterm"
	   "-e"
	   "cdda2wav"
	   "-v" "all"
	   "-B" (concat "-D" cdrom))))
    (set-process-sentinel
     process
     `(lambda (process change)
	(when (file-exists-p (expand-file-name "id" ,dir))
	  (dae-rename-raw ,dir))
	(dae-eject ,cdrom)))))

(defun dae-rename-raw (dir)
  (when (file-exists-p (expand-file-name "id" dir))
    (let* ((default-directory dir)
	   (id (concat dir "id"))
	   (tracks (cddb-parse id 'tracks))
	   (i 1))
      (unless (string= (cddb-parse id 'title) "")
	(while tracks
	  (rename-file
	   (format "audio_%02d.wav" i)
	   (format "%02d-%s.wav" i
		   (dae-quote (pop tracks))))
	  (incf i))
	(let ((target-dir 
	       (expand-file-name (concat (dae-quote (cddb-parse id 'artist))
					 "/"
					 (dae-quote (cddb-parse id 'title)))
				 (concat dae-directory "processing"))))
	  (dae-ensure-directory target-dir)
	  (dolist (file (directory-files dir t))
	    (when (file-regular-p file)
	      (rename-file file (expand-file-name (file-name-nondirectory file)
						  target-dir))))
	  (with-temp-file (expand-file-name "stats" target-dir)
	    (insert (format
		     "Artist: %s\nTitle: %s\nSource: cd\nCDDB: %s\nYear: %s\nTime: %s\n\n"
		     (cddb-parse id 'artist)
		     (cddb-parse id 'title)
		     (cddb-parse id 'cddb)
		     (or (cddb-parse id 'year) "")
		     (format-time-string "%Y%m%dT%H%M%S")))))))))

(defun dae-ensure-directory (dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun dae-quote (name)
  (while (string-match "/" name)
    (setq name (replace-match "-" nil t name)))
  name)

(provide 'dae)

;;; dae.el ends here
