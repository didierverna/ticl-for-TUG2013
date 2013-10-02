;;; package.lisp --- Common Lisp Package definition

;; Copyright (C) 2011 Didier Verna

;; Author:     Didier Verna <didier@lrde.epita.fr>
;; Maintainer: Didier Verna <didier@lrde.epita.fr>

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :cl-user)

(defpackage :com.dvlsoft.ticl
  (:documentation "The TiCL.")
  (:use :cl)
  (:shadow :*readtable*)
  ;; #### PORTME.
  (:import-from #+sbcl :sb-mop
		:class-slots :slot-definition-name :validate-superclass)
  (:import-from :com.dvlsoft.ticl.asdf
   :define-constant
    :+release-major-level+
    :+release-minor-level+
    :+release-status+
    :+release-status-level+
    :+release-name+
    :version)
  (:import-from :tt
   :paragraph)
  (:export
    ;; From com.dvlsoft.ticl.asd:
    :+release-major-level+
    :+release-minor-level+
    :+release-status+
    :+release-status-level+
    :+release-name+
    :version
    ;; From package.lisp:
    :nickname-package
    ;; From lib/ticl.lisp:
    :ticl
    :documentclass
    :title
    :author
    :subject
    :keywords
    :date
    :document
    :maketitle
    :section
    :subsection
    :paragraph))

(defpackage :com.dvlsoft.ticl.user
  (:documentation "The TiCL user package.")
  (:use :com.dvlsoft.ticl))

(in-package :com.dvlsoft.ticl)


;; -------------------
;; External utilities:
;; -------------------

(defun nickname-package (&optional (nickname :ticl))
  "Add NICKNAME (:TICL by default) to the :COM.DVLSOFT.TICL package."
  (rename-package :com.dvlsoft.ticl
		  (package-name :com.dvlsoft.ticl)
		  (adjoin nickname (package-nicknames :com.dvlsoft.ticl)
			  :test #'string-equal)))


;; -------------------
;; Internal utilities:
;; -------------------

(defvar *readtable* (copy-readtable)
  "The TiCL readtable.")

;; String concatenation
;; --------------------
(defun tilde-reader (stream char)
  "Read a series of ~\"string\" to be concatenated together."
  (declare (ignore char))
  (flet ((read-string (&aux (string (read stream t nil t)))
	   (check-type string string "a string")
	   string))
    (apply #'concatenate 'string
	   (read-string)
	   (loop :while (char= (peek-char t stream nil nil t) #\~)
		 :do (read-char stream t nil t)
		 :collect (read-string)))))

(set-macro-character #\~ #'tilde-reader nil *readtable*)

;; Emacs indentation
;; -----------------
(defun clindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
This function sets SYMBOL's common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition.
Otherwise, INDENT is considered as an indentation definition."
  (when (and (member :swank *features*)
	     (boundp 'cl-user::com.dvlsoft.ticl.swank-eval-in-emacs)
	     cl-user::com.dvlsoft.ticl.swank-eval-in-emacs)
    (funcall (intern "EVAL-IN-EMACS" :swank)
	     `(put ',symbol 'common-lisp-indent-function
		   ,(if (symbolp indent)
			`(get ',indent 'common-lisp-indent-function)
		      `',indent))
	     t)))

(defmacro defindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
SYMBOL and INDENT need not be quoted.
See CLINDENT for more information."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Read an argument list for the DEFINDENT macro."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))

(set-dispatch-macro-character #\# #\i #'i-reader *readtable*)


;; ECL and CLISP do not like to see undefined reader macros in expressions
;; that belong to other compilers. For instance this will break:
;; #+ccl (#_ccl-only-function)
;; It seems to be a correct behavior (see *read-suppress* in CLHS), although
;; other implementations like SBCL and CMUCL are more gentle. The solution I
;; use is to define those reader macros to simply return nil.
#+(or ecl clisp)
(progn

  (defun dummy-reader (stream subchar args)
    "Return nil."
    (declare (ignore stream subchar args))
    nil)

  (set-dispatch-macro-character #\# #\_ #'dummy-reader *readtable*)
  (set-dispatch-macro-character #\# #\$ #'dummy-reader *readtable*))

(defmacro in-readtable (name)
  "Set the current readtable to the value of NAME::*READTABLE*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf cl:*readtable* (symbol-value (find-symbol "*READTABLE*" ,name)))))


;;; package.lisp ends here
