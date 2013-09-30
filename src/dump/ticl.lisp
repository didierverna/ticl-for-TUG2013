;;; ticl.lisp --- Standalone program

;; Copyright (C) 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>
;; Created: Mon Sep 30 14:41:09 2013

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

(setq *load-verbose* nil)

(require :asdf
	 #-(or sbcl cmu ccl ecl)
	 '(#p"/usr/local/share/common-lisp/source/asdf/asdf.lisp"))


(asdf:load-system :com.dvlsoft.ticl :verbose nil)
(asdf:load-system :com.dvlsoft.clon :verbose nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.ticl:nickname-package)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "FILE")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
	 (clon:help))
	(t
	 (ticl:ticl (car (clon:remainder)))))
  (clon:exit))

(clon:dump "ticl" main)


;;; ticl.lisp ends here
