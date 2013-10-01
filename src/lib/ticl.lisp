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

(defun ticl::title (title) (setq *title* title))
(defun ticl::author (author) (setq *author* author))
(defun ticl::subject (subject) (setq *subject* subject))
(defun ticl::keywords (keywords) (setq *keywords* keywords))
(defun ticl::date (date) (setq *date* date))

(defun maketitle ()
  (tt:vspace 35)
  (tt:paragraph (:font-size 17.28 :h-align :center) *title*)
  (tt:vspace 15)
  (tt:paragraph (:font-size 12 :h-align :center :bottom-margin 7) *author*)
  (tt:paragraph (:font-size 12 :h-align :center) *date*)
  (tt:vspace 15))


;; Sectionning

;; #### FIXME: the before and after skip in LaTeX classes are specified in
;; ex. I use the magic incantation \newlength\x\x=1ex\showthe\x, but this
;; should really be computed automatically. In this case notably, the actual
;; value for 1ex that I use is okay only when the font is Times-Bold 10pt.
(defvar *section-styles*
  (let ((ex 4.60999)
	(em 10))
    `((:font "Times-Bold" :font-size 14.4
       :top-margin ,(* 3.5 ex) :bottom-margin ,(* 2.3 ex))
      (:font "Times-Bold" :font-size 12
       :top-margin ,(* 3.25 ex) :bottom-margin ,(* 1.5 ex))
      (:font "Times-Bold" :font-size 10
       :top-margin ,(* 3.25 ex) :bottom-margin ,(* 1.5 ex))
      (:font "Times-Bold" :font-size 10
       :top-margin ,(* 3.25 ex) :bottom-margin ,(* 1.5 em))
      (:font "Times-Bold" :font-size 10
       :top-margin ,(* 3.25 ex) :bottom-margin ,(* 1.5 em))
      (:font "Times-Bold" :font-size 10
       :top-margin ,(* 3.25 ex) :bottom-margin ,(* 1.5 em)))))

(defvar *section-number*)

(defun section-number-string (section-number)
  (format nil "~{~S~^.~}" section-number))

(defmacro %section (level name)
  `(tt:paragraph ,(nth level *section-styles*)
     (tt:put-string
      (section-number-string ',(subseq *section-number* 0 (1+ level))))
     (tt:hspace 10) ;; #### FIXME: this should be 1em in the current font.
     ,name))

(defmacro section (name)
  `(progn
     (incf (car *section-number*))
     (setf (cadr *section-number*) 0)
     (%section 0 ,name)))

(defmacro subsection (name)
  `(progn
     (incf (cadr *section-number*))
     (%section 1 ,name)))


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
	  :hfill))))
  (tt:compile-text () ""))

;; Modified from kw-extensions to:
;; - don't use the cl-typesetting package (instead, use prefix),
;; - render to *OUTPUT-FILE* by default,
;; - remove the TWOSIDED and PAPER-SIZE keys,
;; - only display the page number in footer, as in LaTeX's plain style,
;; - fill in PDF meta-data.
(defun render-document (trees &key (file *output-file*))
  "Render the document specified by the trees, which is a s-exp containing
a list of recursive typesetting commands. It gets eval'ed here to typeset it."
  (setq *section-number* '(0 0))
  (setq cl-typesetting-hyphen::*left-hyphen-minimum* 999
	cl-typesetting-hyphen::*right-hyphen-minimum* 999)
  (tt:with-document (:title *title*
		     :author *author*
		     :subject *subject*
		     :keywords *keywords*)
    (dolist (tree trees)
      (tt:draw-pages
       (eval `(tt:compile-text ()
		(tt:with-style ,tt::*default-text-style*
		  (tt:set-style ,(tt:get-contextual-variable :style ()))
		  (tt:set-contextual-variable :footer-enabled t)
		  ,tree)))
       :margins tt::*page-margins* ; why isn't that a default ?!
       :footer #'footer))
    (when pdf:*page* (tt:finalize-page pdf:*page*))
    (when (and (tt::final-pass-p)
	       tt::*undefined-references*)
      (format t "Undefined references:~%~S~%"
	      tt::*undefined-references*))
    (pdf:write-document file)))

(defun ticl (file)
  "Run TiCL on FILE."
  (setq *output-file* (merge-pathnames (make-pathname :type "pdf") file)
	;; #### NOTE: There are other interesting parameters.
	tt::*default-font* "Times-Roman" tt::*font* tt::*default-font*
	tt::*default-font-size* 10.0 tt::*font-size* tt::*default-font-size*
	tt::*default-h-align* :justified tt::*h-align* tt::*default-h-align*
	tt::*default-v-align* :justified tt::*v-align* tt::*default-v-align*
	tt::*paper-size* :letter
	tt::*default-page-size* :letter
	tt::*page-margins* '(134.26999 125.26999 134.73001 118.72998)
	tt::*default-page-header-footer-margin* 88.72998
	tt::*twosided* nil  ;; t by default
	cl-pdf::*name-counter* 0) ; this one seems to be a bug.
  (load file))


;;; ticl.lisp ends here
