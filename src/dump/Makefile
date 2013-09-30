### Makefile --- Dump directory

## Copyright (C) 2011 Didier Verna

## Author:     Didier Verna <didier@lrde.epita.fr>
## Maintainer: Didier Verna <didier@lrde.epita.fr>

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

TOP_DIR := ../..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc


BINS := ticl

all: $(BINS)
ifeq ($(LISP),ABCL)
# ABCL is java based, so we produce a jar, and a wrapper shell script around
# it.
$(BINS): %: %.jar
	echo '#! /bin/sh' > $@
	echo '$(JAVA) -jar $< $${1+$$@}' >> $@
	chmod 755 $@
else ifeq ($(LISP),ECL)
# ECL also has a different scheme for producing executables.
$(BINS): %: %.lisp
	$($(LISP)_PATH) $($(LISP)_DUMP) dump.lisp -- $@
else
# The default way of doing things in pure lisp:
$(BINS): %: %.lisp
	$($(LISP)_PATH) $($(LISP)_DUMP) $<
endif


# --------------------------
# Java (ABCL) specific part:
# --------------------------

# This upcases the first letter of $(1):
java_name = $(shell echo $(1) | $(SED) 's/\([a-z]\)\([a-zA-Z0-9]*\)/\u\1\2/')
# This defines how to create foo.jar from its dependencies Foo.txt and
# Foo.class. Note the additional class file ending in $1.class. That's because
# of the Runnable inner class defined by the dumping macro. We need to delete
# it by hand because it's not seen as a target/dependency by make so
# .INTERMEDIATE doesn't work on it.
define jar_rule
$(1).jar: $(call java_name,$(1)).txt $(call java_name,$(1)).class $(1).lisp
	cp $(ABCL_JAR) $$@
	$(JAR) -ufm $$@ $$^ $(call java_name,$(1))\$$$$1.class $(1).lisp
	rm $(call java_name,$(1))\$$$$1.class
endef
# This defines how to create Foo.java from foo.lisp:
define java_rule
$(call java_name,$(1).java): $(1).lisp
	$(ABCL_PATH) $(ABCL_DUMP) $$<
endef

$(foreach bin,$(BINS),$(eval $(call jar_rule,$(bin))))
$(foreach bin,$(BINS),$(eval $(call java_rule,$(bin))))
$(foreach bin,$(BINS),$(call java_name,$(bin).txt)):
	echo "Main-Class: $(@:%.txt=%)" > $@

%.class: %.java
	$(JAVAC) -cp $(ABCL_JAR) $<


# ---------
# The rest:
# ---------

clean:
	-rm *~ $(BINS:%=%.o) # this last bit is for ECL

distclean: clean
	-rm -fr $(BINS) $(BINS:%=%.jar)

install:
uninstall:
install-www:

.INTERMEDIATE: $(foreach bin,$(BINS), $(call java_name,$(bin)).txt)	 \
	       $(foreach bin,$(BINS), $(call java_name,$(bin)).java)	 \
	       $(foreach bin,$(BINS), $(call java_name,$(bin)).class)

.PHONY: hack all clean distclean install uninstall install-www


### Makefile ends here
