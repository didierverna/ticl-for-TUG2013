;;; generate.cl --- TiCL reference manual generation script

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

(require :asdf)

(defconstant +introduction+
  "@macro ticl
@t{TiCL}
@end macro

@macro cl
Common-Lisp
@end macro

@ticl{} is a @cl{} implementation of @TeX{}."
  "The reference manual's introductory text.")

(asdf:operate 'asdf:load-op :com.dvlsoft.declt)

(if (and (second sb-ext:*posix-argv*)
	 (string= (second sb-ext:*posix-argv*) "--web"))
    (com.dvlsoft.declt:declt :com.dvlsoft.ticl
			     :library-name "TiCL"
			     :texi-file "webreference.texi"
			     ;; but we don't care
			     :info-file "ticl-webreference"
			     :introduction +introduction+
			     :license :gpl
			     :copyright-date "2011"
			     :link-files nil)
  (com.dvlsoft.declt:declt :com.dvlsoft.ticl
			   :library-name "TiCL"
			   :texi-file "reference.texi"
			   :info-file "ticl-reference"
			   :introduction +introduction+
			   :license :gpl
			   :copyright-date "2011"))

(sb-ext:quit)


;;; generate.cl ends here
