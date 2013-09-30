;;; com.dvlsoft.ticl.asd --- ASDF system definition

;; Copyright (C) 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>
;; Created: Mon Sep 30 14:43:53 2013

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

(in-package :cl-user)

(defpackage :com.dvlsoft.ticl.asdf
  (:documentation "The TiCL package for ASDF.")
  (:use :cl)
  (:export :define-constant
	   :+release-major-level+
	   :+release-minor-level+
	   :+release-status+ :+release-status-level+
	   :+release-name+
	   :version))


(in-package :com.dvlsoft.ticl.asdf)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defconstant +release-major-level+ 0
  "The major level of this release.")

(defconstant +release-minor-level+ 1
  "The minor level of this release.")

(defconstant +release-status+ :alpha
  "The status of this release.")

(defconstant +release-status-level+ 1
  "The status level of this release.")

(define-constant +release-name+ "TBD"
  "The name of this release.")

;; #### TODO: I'm sure the format strings can be improved
(defun %version (type major minor status level name)
  (ecase type
    (:number
     (apply #'+
       (* major 10000)
       (* minor 100)
       (when (eq status :patchlevel)
	 (list level))))
    (:short
     (format nil "~S.~S~
		 ~[~
		   a~*~S~;~
		   b~*~S~;~
		   rc~*~S~;~
		   ~:[.~S~;~*~]~
		 ~]"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level))
    (:long
     (format nil "~S.~S ~
		 ~[~
		   alpha ~*~S ~;~
		   beta ~*~S ~;~
		   release candidate ~*~S ~;~
		   ~:[patchlevel ~S ~;~*~]~
		 ~]~
		 ~S"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level
       name))))

(defun version (&optional (type :number))
  "Return the current version of TiCL.
TYPE can be one of :number, :short or :long.

A version number is computed as major*10000 + minor*100 + patchlevel, leaving
two digits for each level. Alpha, beta and rc status are ignored in version
numbers.

A short version is something like 1.3{a,b,rc}4, or 1.3.4 for patchlevel.
Alpha, beta or rc levels start at 1. Patchlevels start at 0 but are ignored
in the output, so that 1.3.0 appears as just 1.3.

A long version is something like
1.3 {alpha,beta,release candidate,patchlevel} 4 \"Michael Brecker\". As for
the short version, a patchlevel of 0 is ignored in the output."
  (%version type +release-major-level+ +release-minor-level+
	    +release-status+ +release-status-level+
	    +release-name+))

(asdf:defsystem :com.dvlsoft.ticl
  :long-name "TeX in Common Lisp."
  :description "A Tex-like typesetting system written in Common Lisp"
  :long-description "Ticl is intended to be a typesetting system with the
typographic quality of TeX and the programming power of Lisp."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  ;; :homepage ""
  ;; :source-control ""
  :license "GNU GPL"
  :version #.(version :long)
  :depends-on (:cl-typesetting)
  :components ((:file "package")
	       (module "lib"
		       :depends-on ("package")
		       :serial t
		       :components ((:file "ticl")))))


;;; com.dvlsoft.ticl.asd ends here
