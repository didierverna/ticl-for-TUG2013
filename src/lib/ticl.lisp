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

(defvar *output-file*)
(defvar *title*)
(defvar *author*)
(defvar *subject*)
(defvar *keywords*)
(defvar *date*)

;; Modified from kw-extensions to:
;; - not add a final dot to section numbers.
(defun chapter-markup (level heading &optional content)
  (let* ((ref-id (tt::new-chp-ref level heading))
	 (cprefix (if tt::*add-chapter-numbers*
		      (concatenate 'string (tt::chpnum-string (cdr ref-id)) " ")
		      ""))
	 (numbered-heading (concatenate 'string cprefix heading)))
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
	 ,@(if (null content)
	       (list heading)
	       content)))))

;; Modified from kw-extensions to:
;; - don't use the cl-typesetting package (instead, use prefix),
;; - render to *OUTPUT-FILE* by default,
;; - remove the TWOSIDED and PAPER-SIZE keys,
;; - fill in PDF meta-data.
(defun render-document (trees &key (file *output-file*))
  "Render the document specified by the trees, which is a s-exp containing
a list of recursive typesetting commands. It gets eval'ed here to typeset it."
  (setq cl-typesetting-hyphen::*left-hyphen-minimum* 999
	cl-typesetting-hyphen::*right-hyphen-minimum* 999)
  (tt:with-document (:title *title*
		     :author *author*
		     :subject *subject*
		     :keywords *keywords*)
    (let ((margins tt::*page-margins*)
	  (header (lambda (pdf:*page*)
		    (if (tt:get-contextual-variable :header-enabled)
			(let ((inside (tt:get-contextual-variable
				       :title "*Untitled Document*"))
			      (outside (tt:get-contextual-variable
					:chapter "*No Chapter*")))
			  (if (and tt::*twosided* (evenp pdf:*page-number*))
			      (tt:compile-text ()
				(tt:with-style (:font-size 10
						:pre-decoration :none
						:post-decoration :none)
				  (tt:hbox (:align :center :adjustable-p t)
				    (tt:with-style (:font tt::*font-normal*)
				      (tt:put-string outside))
				    :hfill
				    (tt:with-style (:font tt::*font-italic*)
				      (tt:put-string inside)))))
			      (tt:compile-text ()
				(tt:with-style (:font-size 10
						:pre-decoration :none
						:post-decoration :none)
				  (tt:hbox (:align :center :adjustable-p t)
				    (tt:with-style (:font tt::*font-italic*)
				      (tt:put-string inside))
				    :hfill
				    (tt:with-style (:font tt::*font-normal*)
				      (tt:put-string outside)))))))
			(tt:compile-text () ""))))
	  (footer (lambda (pdf:*page*)
		    (if (tt:get-contextual-variable :footer-enabled)
			(let ((inside (tt:get-contextual-variable :version ""))
			      (outside (format nil "Page ~d of ~d"
					       pdf:*page-number*
					       (tt:find-ref-point-page-number
						"DocumentEnd"))))
			  (if (and tt::*twosided* (evenp pdf:*page-number*))
			      (tt:compile-text ()
				(tt:with-style (:font tt::*font-normal*
						:font-size 10
						:pre-decoration :none
						:post-decoration :none)
				  (tt:hbox (:align :center :adjustable-p t)
				    (tt:put-string outside)
				    :hfill
				    (tt:put-string inside))))
			      (tt:compile-text ()
				(tt:with-style (:font tt::*font-normal*
						:font-size 10
						:pre-decoration :none
						:post-decoration :none)
				  (tt:hbox (:align :center :adjustable-p t)
				    (tt:put-string inside)
				    :hfill
				    (tt:put-string outside))))))
			(tt:compile-text () "")))))
      (tt:set-contextual-variable :header-enabled nil)
      (tt:set-contextual-variable :footer-enabled nil)
      (tt::set-contextual-style (:pre-decoration :none))
      (dolist (tree trees)
	(tt:draw-pages
	 (eval `(tt:compile-text ()
		  (tt:with-style ,tt::*default-text-style*
		    (tt:set-style ,(tt:get-contextual-variable :style ()))
		    ,tree)))
	 :margins margins
	 :header header
	 :footer footer
	 :size tt::*paper-size*
	 :finalize-fn #'tt::page-decorations))
      (when pdf:*page* (tt:finalize-page pdf:*page*))
      (when (and (tt::final-pass-p)
		 tt::*undefined-references*)
	(format t "Undefined references:~%~S~%"
		tt::*undefined-references*))
      (pdf:write-document file))))

;; Cf. *default-text-style* for font-size and *chapter-styles* for headers.

(defun ticl (file)
  "Run TiCL on FILE."
  (setq *output-file* (merge-pathnames (make-pathname :type "pdf") file)
	tt::*default-h-align* :justified
	tt::*h-align* :justified
	;; #### There are other interesting parameters.
	tt::*verbose* t
	tt::*paper-size* :a4 ;; :letter by default
	tt::*page-margins* '(72 72 72 50)
	tt::*twosided* nil  ;; t by default
	tt::*toc-depth* 3
	cl-pdf::*name-counter* 0)
  (load file)
  ;; #### FIXME: this is for the TOC. Should use TT::FINAL-PASS-P instead.
  (load file))


;;; ticl.lisp ends here
