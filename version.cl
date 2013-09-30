;;; version.cl --- TiCL version extractor script

;; Copyright (C) 2011 Didier Verna

;; Author:     Didier Verna <didier@lrde.epita.fr>
;; Maintainer: Didier Verna <didier@lrde.epita.fr>

;; This file is part of TiCL.

;; Ticl is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Ticl is distributed in the hope that it will be useful,
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

(require :asdf)

(asdf:operate 'asdf:load-op :com.dvlsoft.ticl)
(com.dvlsoft.ticl:nickname-package)

(format t "LONG_VERSION  := ~A~%~
	   SHORT_VERSION := ~A~%"
  (ticl:version :long)
  (ticl:version :short))

;; #### PORTME.
(sb-ext:quit)


;;; version.cl ends here
