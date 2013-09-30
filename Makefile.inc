### Makefile.inc --- Inclusion part

## Copyright (C) 2011 Didier Verna

## Author:     Didier Verna <didier@lrde.epita.fr>
## Maintainer: Didier Verna <didier@lrde.epita.fr>

## This file is part of TiCL.

## TiCL is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 3,
## as published by the Free Software Foundation.

## TiCL is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

PROJECT   := ticl
PACKAGE   := com.dvlsoft.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/$(PROJECT)

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_DUMP   := --script

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_LOAD   := -load
CMUCL_DUMP   := -noinit -nositeinit $(CMUCL_LOAD)

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_DUMP   := --no-init $(CCL_LOAD)

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_LOAD   := -load
ECL_DUMP   := -norc $(ECL_LOAD)

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_LOAD   := -i
CLISP_DUMP   := -norc $(CLISP_LOAD)

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_LOAD   := --load
ABCL_DUMP   := --batch --eval '(defvar cl-user::$(PACKAGE).dump t)' \
	       $(ABCL_LOAD)

$(TOP_DIR)/version.inc: $(TOP_DIR)/version.cl $(TOP_DIR)/src/$(ASDF_FILE)
	$($(LISP)_PATH) $($(LISP)_LOAD) $(TOP_DIR)/version.cl | tail -2 > $@


### Makefile.inc ends here
