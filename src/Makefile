### Makefile --- Source directory

## Copyright (C) 2011 Didier Verna

## Author:     Didier Verna <didier.verna@gmail.com>
## Maintainer: Didier Verna <didier.verna@gmail.com>

## This file is part of TiCL.

## TiCL is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

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


SUBDIRS := lib dump


gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done


.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: hack gen


### Makefile ends here
