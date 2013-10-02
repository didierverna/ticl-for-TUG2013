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


(defvar *em* 10)
(defvar *ex* 4.49998)
(defvar *em-bold* 10)
(defvar *ex-bold* 4.49998)

(defun maketitle ()
  (tt:vspace 35)
  (tt:paragraph (:font-size (large) :h-align :center)
    *title*)
  (tt:vspace 15)
  (tt:paragraph (:font-size (|large|) :h-align :center :bottom-margin 7)
    *author*)
  (tt:paragraph (:font-size (|large|) :h-align :center)
    *date*)
  (tt:vspace 15))

;; Sectionning

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

;; #### WARNING: there's a bug somewhere which makes the reinitialization of
;; this variable inoperative, no matter where I do it. It's a matter of
;; (PRINT (SETQ X 1)) -> != 1 and I have no idea what's going on. Happens with
;; many Lisp engines as well.
(defvar *section-number* '(0 0))

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

(defmacro %section (level name)
  `(let* ((section-number-string
	    (progn (increment-section-number ,level)
		   (section-number-string (section-number ,level))))
	  (section-reference-string
	    (section-reference-string section-number-string)))
     (pdf:with-outline-level
	 ((concatenate 'string section-number-string " " ,name)
	  (pdf::register-named-reference
	   (vector
	    (tt::find-ref-point-page-content (print section-reference-string))
	    "/Fit")))
       (tt:paragraph ,(nth level (section-styles))
	 (tt:mark-ref-point (print section-reference-string) :data ,name
						     :page-content t)
	 (tt:put-string section-number-string)
	 (tt:hspace 10) ;; #### FIXME: this should be 1em in the current font.
	 ,name))))

(defmacro paragraph (&rest args)
  (if (stringp (car args))
      `(tt:paragraph () ,@args)
      `(tt:paragraph ,@args)))

(defmacro subsection (name)
  `(%section 1 ,name))

(defmacro section (name)
  `(%section 0 ,name))

;; Modified from kw-extensions to:
;; - not add a final dot to section numbers.
(defun chapter-markup (level heading &optional content)
  (let* ((ref-id (tt::new-chp-ref level heading))
	 (cprefix (if tt::*add-chapter-numbers*
		      (concatenate 'string (tt::chpnum-string (cdr ref-id)))
		      ""))
	 (numbered-heading (concatenate 'string cprefix " " heading)))
    `(pdf:with-outline-level
	 (,numbered-heading
	  (pdf::register-named-reference
	   (vector (tt::find-ref-point-page-content ',ref-id) "/Fit")
	   ,(pdf::gen-name "R")))
       ,(if (eql level 0) :fresh-page "")
       ,(if (eql level 0) `(tt:set-contextual-variable :chapter ,heading) "")
       (tt:paragraph ,(nth level tt::*chapter-styles*)
	 (tt:mark-ref-point ',ref-id :data ,heading :page-content t)
	 (tt:put-string ,cprefix)
	 (tt:hspace 10) ;; #### FIXME: this should be 1em in the current font.
	 ,@(if (null content)
	       (list heading)
	       content)))))

(defvar *documentclass* :article)
(defun documentclass (class &key (paper :letter) (pt 10))
  (setq *documentclass* class
	tt::*paper-size* paper tt::*default-page-size* paper
	tt::*default-font-size* pt tt::*font-size* tt::*default-font-size*))

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

(defmacro document (&body body)
  `(tt:with-document (:title *title*
		      :author *author*
		      :subject *subject*
		      :keywords *keywords*)
     (tt:draw-pages
      (tt:compile-text () ,@body)
      :margins tt::*page-margins* ; why isn't that a default ?!
      :footer #'footer)
     (when pdf:*page* (tt:finalize-page pdf:*page*))
     (when (and (tt::final-pass-p)
		tt::*undefined-references*)
       (format t "Undefined references:~%~S~%"
	       tt::*undefined-references*))
     (pdf:write-document *output-file*)))

(defun ticl (file)
  "Run TiCL on FILE."
  (setq *output-file* (merge-pathnames (make-pathname :type "pdf") file)
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
  (let ((*package* (find-package :com.dvlsoft.ticl.user))
	;; Ideally, this should be part of WITH-DOCUMENT:
	(*section-number* '(0 0)))
    (load file)))


;;; ticl.lisp ends here
