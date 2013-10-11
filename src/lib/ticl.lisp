;;; ticl.lisp --- TiCL's entry point

;; Copyright (C) 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>
;; Created: Mon Sep 30 15:20:44 2013

;; This file is part of TiCL.

;; TiCL is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; TiCL is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:



;;; Code:


(in-package   :com.dvlsoft.ticl)
(in-readtable :com.dvlsoft.ticl)


;; Document meta-data and title management

(defvar *output-file*)
(defvar *toc-file*)
(defvar *toc*)

(defvar *title*)
(defvar *author*)
(defvar *subject*)
(defvar *keywords*)
(defvar *date*)

(defun title (title) (setq *title* title))
(defun author (author) (setq *author* author))
(defun subject (subject) (setq *subject* subject))
(defun keywords (keywords) (setq *keywords* keywords))
(defun date (date) (setq *date* date))

(defun large ()
  (cond ((or (= tt::*default-font-size* 10) (= tt::*default-font-size* 11))
	 17.28)
	(t 20.74)))

(defun |Large| ()
  (cond ((or (= tt::*default-font-size* 10) (= tt::*default-font-size* 11))
	 14.4)
	(t 17.28)))

(defun |large| ()
  (cond ((or (= tt::*default-font-size* 10) (= tt::*default-font-size* 11))
	 12)
	(t 14.4)))


(defparameter *em* 10)
(defparameter *ex* 4.49998)
(defparameter *em-bold* 10)
(defparameter *ex-bold* 4.49998)

(define-constant +paper-sizes+ '((:letter 614.295 794.96999)
				 (:a4 597.50787 845.04684)))

(defmacro textbf (&body body)
  `(tt:with-style (:font "Times-Bold") ,@body))

(defmacro textit (&body body)
  `(tt:with-style (:font "Times-Italic") ,@body))


;; Sectionning

(defparameter *parindent* 20)
(defparameter *indent-first-line* *parindent*)

;; #### FIXME: the before and after skip in LaTeX classes are specified in
;; ex. I use the magic incantation \newlength\x\x=1ex\showthe\x, but this
;; should really be computed automatically. In this case notably, the actual
;; value for 1ex that I use is okay only when the font is Times-Bold 10pt.
(defun section-styles ()
  `((:font "Times-Bold" :font-size (|Large|)
     :top-margin ,(* 3.5 *ex-bold*) :bottom-margin ,(* 2.3 *ex-bold*))
    (:font "Times-Bold" :font-size (|large|)
     :top-margin ,(* 3.25 *ex-bold*) :bottom-margin ,(* 1.5 *ex-bold*))
    (:font "Times-Bold" :font-size tt::*default-font-size*
     :top-margin ,(* 3.25 *ex-bold*) :bottom-margin ,(* 1.5 *ex-bold*))
    (:font "Times-Bold" :font-size tt::*default-font-size*
     :top-margin ,(* 3.25 *ex-bold*) :bottom-margin ,(* 1.5 *em-bold*))
    (:font "Times-Bold" :font-size tt::*default-font-size*
     :top-margin ,(* 3.25 *ex-bold*) :bottom-margin ,(* 1.5 *em-bold*))
    (:font "Times-Bold" :font-size tt::*default-font-size*
     :top-margin ,(* 3.25 *ex-bold*) :bottom-margin ,(* 1.5 *em-bold*))))

(defvar *section-number*)

(defun section-number-string (section-number)
  (format nil "~{~S~^.~}" section-number))

(defun section-number (level)
  (subseq *section-number* 0 (1+ level)))

(defun section-reference-string (section-number-string)
  (format nil "section ~A" section-number-string))

(defun increment-section-number (level)
  (cond ((= level 0)
	 (incf (car *section-number*))
	 (setf (cadr *section-number*) 0))
	((= level 1)
	 (incf (cadr *section-number*)))))

(defun toc-line (level section-number-string name section-reference-string)
  `(tt:paragraph (:h-align :left-but-last
		  :font ,(case level
			   (0 tt::*font-bold*)
			   (t tt::*font-normal*))
		  ;; #### FIXME: these should be computed based on the current
		  ;; value of 1em.
		  :left-margin ,(case level
				  (0 0)
				  (1 15)
				  (2 38)
				  (3 70)
				  (t 100))
		  :top-margin  ,(if (> level 0) 0 10))
     (tt:put-string ,section-number-string)
     (tt:hspace ,(case level
		   (0 10)
		   (1 12.5)
		   ;; #### FIXME: these are wrong.
		   (2 41)
		   (t 50)))
     (tt:put-string ,name)
     ,(if (= level 0)
	  :hfill
	  '(tt::dotted-hfill))
     (tt:with-style (:font-size tt::*default-font-size*)
       (tt::put-ref-point-page-number ,section-reference-string))))

;; #### WARNING: this whole paragraph thing is an awful kludge, just here to
;; exercise the "parindent except section's first ones" feature. In
;; particular, we must be careful to never use plain strings in our internal
;; functions (and use tt::put-string directly instead) in order to prevent
;; unwanted effects on *PARAGRAPH-START*. AAMOF, I'm thinking that I shouldn't
;; use tt:paragraph at all in here (but maybe only tt:with-style) because it's
;; too high-level.
(defparameter *paragraph-start* t)

(defun par ()
  (tt::new-line)
  (setq *paragraph-start* t)
  "")
(define-symbol-macro par (par))

(defmacro with-par (&body body)
  `(progn
     ,@(mapcar 'tt::insert-stuff body)
     (par)))

(defun put-simple-string (string)
  (when *paragraph-start*
    (unless (zerop *indent-first-line*)
      (tt::add-box (make-instance 'tt::h-spacing :dx *indent-first-line*)))
    (setq *indent-first-line* *parindent*
	  *paragraph-start* nil))
  (tt::put-string string))

(defun put-string (string)
  (loop :with len := (length string)
	:and start := 0
	:and look := 0
	:and eol1 :and eol2p
	:until (= start len)
	:do (setq eol1 (position #\Newline string :start look)
		  eol2p (and eol1
			     (< eol1 (1- len))
			     (eq (elt string (1+ eol1)) #\Newline )))
	:if (and eol1 eol2p)
	  :do (progn (put-simple-string (subseq string start eol1))
		     (par)
		     (setq start (+ eol1 2)
			   look start))
	:else
	  :if (or (not eol1) (= eol1 (1- len)))
	    :do (progn (put-simple-string (subseq string start))
		       (setq start len))
	:else
	  :do (setq look (1+ eol1))))

(defmethod tt::insert-stuff ((obj string))
  `(put-string ,obj))


(defmacro %with-section (level name &body body)
  `(let* ((section-number-string
	    (progn (increment-section-number ,level)
		   (section-number-string (section-number ,level))))
	  (section-reference-string
	    (section-reference-string section-number-string)))
     (push
      (toc-line ,level section-number-string ,name section-reference-string)
      *toc*)
     (pdf:with-outline-level
	 (,name
	  (pdf::register-named-reference
	   (vector
	    (tt::find-ref-point-page-content section-reference-string)
	    "/Fit")))
       (par)
       (tt:paragraph ,(nth level (section-styles))
	 (tt:mark-ref-point section-reference-string :data ,name
						     :page-content t)
	 (tt:put-string section-number-string)
	 (tt:hspace 10) ;; #### FIXME: this should be 1em in the current font.
	 (tt:put-string ,name))
       (setq *indent-first-line* 0
	     *paragraph-start* t)
       ,@(mapcar 'tt::insert-stuff body)
       (par))))

(defmacro with-subsection (name &body body)
  `(%with-section 1 ,name ,@body))

(defmacro with-section (name &body body)
  `(%with-section 0 ,name ,@body))


(defmacro %section (level name)
  `(let* ((section-number-string
	    (progn (increment-section-number ,level)
		   (section-number-string (section-number ,level))))
	  (section-reference-string
	    (section-reference-string section-number-string)))
     (par)
     (push
      (toc-line ,level section-number-string ,name section-reference-string)
      *toc*)
     ;; Ouch! We miss an unwind-protect here, from pdf:with-outline-level.
     (pdf:enter-outline-level
      ,name
      (pdf::register-named-reference
       (vector
	(tt::find-ref-point-page-content section-reference-string)
	"/Fit")))
     (tt:paragraph ,(nth level (section-styles))
       (tt:mark-ref-point section-reference-string :data ,name
						   :page-content t)
       (tt:put-string section-number-string)
       (tt:hspace 10) ;; #### FIXME: this should be 1em in the current font.
       (tt:put-string ,name))
     (setq *indent-first-line* 0
	   *paragraph-start* t)))

;; #### NOTE: In a real world application, this would be generalized as a
;; stack of opened sections of different levels.
(defmacro subsection (name)
  `(progn
     (unless (zerop (cadr *section-number*))
       (par)
       (pdf:close-outline-level))
     (%section 1 ,name)))

(defmacro section (name)
  `(progn
     (unless (zerop (cadr *section-number*))
       (par)
       (pdf:close-outline-level))
     (unless (zerop (car *section-number*))
       (par)
       (pdf:close-outline-level))
     (%section 0 ,name)))


(defmacro table-of-contents ()
  `(when (probe-file *toc-file*)
     (tt:paragraph ,(append
		     ;; Overwrite bottom margin to compensate for the
		     ;; additional vertical space of level 0 section
		     ;; headers.
		     '(:bottom-margin (* .5 *ex-bold*))
		     (nth 0 (section-styles)))
       (tt::put-string "Contents"))
     (load *toc-file*)
     ""))
(define-symbol-macro tableofcontents (table-of-contents))

(defun make-title ()
  (tt:vspace 35)
  (tt:paragraph (:font-size (large) :h-align :center)
    (tt::put-string *title*))
  (tt:vspace 15)
  (tt:paragraph (:font-size (|large|) :h-align :center :bottom-margin 7)
    (tt::put-string *author*))
  (tt:paragraph (:font-size (|large|) :h-align :center)
    (tt::put-string *date*))
  (tt:vspace 15)
  "")
(define-symbol-macro maketitle (make-title))

(defparameter *document-class* :article)

(defun document-class (class &key (paper :letter) (pt 10))
  (setq *document-class* class
	tt::*paper-size* paper
	tt::*default-page-size* tt::*paper-size*
	tt::*default-font-size* pt
	tt::*font-size* tt::*default-font-size*))

(defmacro documentclass (class &key (paper :letter) (pt 10))
  (let ((the-class (intern (symbol-name class) :keyword))
	(the-paper (intern (symbol-name paper) :keyword)))
    `(document-class ,the-class :paper ,the-paper :pt ,pt)))


(defun footer (pdf:*page*)
  (let ((pagenum (format nil "~d" pdf:*page-number*)))
    (tt:compile-text ()
      (tt:with-style (:font tt::*default-font*
		      :font-size tt::*default-font-size*
		      :pre-decoration :none
		      :post-decoration :none)
	(tt:hbox (:align :center :adjustable-p t)
	  :hfill
	  (tt:put-string pagenum)
	  :hfill)))))

(defmacro with-document (&body body)
  `(tt:with-document (:title *title*
		      :author *author*
		      :subject *subject*
		      :keywords *keywords*)
     ;; #### WARNING: For some reason that I don't understand, setting
     ;; *SECTION-NUMBER* to a constant '(0 0) doesn't work. It doesn't get
     ;; reinitialized.
     (setq *section-number* (list 0 0)
	   *toc* nil)
     (tt:draw-pages
      (tt:compile-text ()
	,@body
	(unless (zerop (cadr *section-number*))
	  (par)
	  (pdf:close-outline-level))
	(unless (zerop (car *section-number*))
	  (par)
	  (pdf:close-outline-level)))
      :margins tt::*page-margins* ; why isn't that a default ?!
      :footer #'footer)
     (when pdf:*page* (tt:finalize-page pdf:*page*))
     (when (and (tt::final-pass-p)
		tt::*undefined-references*)
       (format t "Undefined references:~%~S~%"
	       tt::*undefined-references*))
     (with-open-file (toc *toc-file* :direction :output
				     :if-exists :overwrite
				     :if-does-not-exist :create)
       (mapc (lambda (toc-line) (format toc "~S~%" toc-line))
	     (reverse *toc*)))
     (pdf:write-document *output-file*)))


(defmacro begin (thing)
  `(,(intern (concatenate 'string "BEGIN-" (symbol-name thing))
	     :com.dvlsoft.ticl)))

(defmacro end (thing)
  `(,(intern (concatenate 'string "END-" (symbol-name thing))
	     :com.dvlsoft.ticl)))

(let ((magick (gensym)))
  (defun open-environment (stream subchar arg)
    (declare (ignore subchar arg))
    (let ((name (read stream)))
      (cons (intern (concatenate 'string "WITH-" (symbol-name name))
		    :com.dvlsoft.ticl.user)
	    (loop :for object := (read stream)
		  :until (when (and (consp object) (eq (car object) magick))
			   (unless (eq name (cdr object))
			     (error "~A environment ended with ~A."
				    name (cdr object)))
			   t)
		  :collect object))))
  (defun close-environment (stream subchar arg)
    (declare (ignore subchar arg))
    (let ((name (read stream)))
      (cons magick name))))

(defun make-user-readtable ()
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\{ #'open-environment readtable)
    (set-dispatch-macro-character #\# #\} #'close-environment readtable)
    readtable))

(defparameter *user-readtable* (make-user-readtable))

(defun convert (file)
  (with-open-file (stream file :direction :input)
    (with-output-to-string (string)
      (let ((*package* (find-package :com.dvlsoft.ticl.user))
	    (*readtable* *user-readtable*)
	    (*print-readably* t)
	    (*print-pretty* nil))
	(loop :with in-string := nil
	      :for char := (read-char stream nil stream)
	      :until (eq char stream)
	      :if (eq char #\\)
		:do (progn (when in-string
			     (write-char #\" string)
			     (setq in-string nil))
			   (let ((expr (read-preserving-whitespace stream)))
			     (if (consp expr)
				 (cond ((eq (car expr)
					    'com.dvlsoft.ticl.user::begin)
					(format string "#{~S" (cadr expr)))
				       ((eq (car expr)
					    'com.dvlsoft.ticl.user::end)
					(format string "#}~S" (cadr expr)))
				       (t
					(prin1 expr string)))
				 (prin1 expr string))))
	      :else
		:do (progn (unless in-string
			     (write-char #\" string)
			     (setq in-string t))
			   (write-char char string))
	      :finally (when in-string (write-char #\" string)))
	string))))

(defun ticl (file)
  "Run TiCL on FILE."
  (setq *output-file* (merge-pathnames (make-pathname :type "pdf") file)
	*toc-file* (merge-pathnames (make-pathname :type "toc") file)
	;; #### NOTE: There are other interesting parameters.
	tt::*default-font* (pdf:get-font "Times-Roman")
	tt::*font* tt::*default-font*
	tt::*default-font-size* 10 tt::*font-size* tt::*default-font-size*
	tt::*default-h-align* :justified tt::*h-align* tt::*default-h-align*
	tt::*default-v-align* :justified tt::*v-align* tt::*default-v-align*
	tt::*paper-size* :letter
	tt::*default-page-size* :letter
	tt::*page-margins* '(134.26999 125.26999 134.73001 118.72998)
	tt::*default-page-header-footer-margin* 88.72998
	tt::*twosided* nil  ;; t by default
	cl-pdf::*name-counter* 0 ; this one seems to be a bug.
	cl-typesetting-hyphen::*left-hyphen-minimum* 999
	cl-typesetting-hyphen::*right-hyphen-minimum* 999)
  (let ((stream (if (string= "ltic" (subseq file (- (length file) 4)))
		    (make-string-input-stream (convert file))
		    file))
	(*package* (find-package :com.dvlsoft.ticl.user))
	(*readtable* *user-readtable*))
    (load stream)))


;;; ticl.lisp ends here
