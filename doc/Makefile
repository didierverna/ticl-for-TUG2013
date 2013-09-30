### Makefile --- Doc directory

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

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc
include $(TOP_DIR)/version.inc
ifneq ($(SBCL_PATH),)
  include declt.inc
endif

INFO_DIR        := $(PREFIX)/share/info
DOC_DIR	        := $(SHARE)/doc
PROJECT_DOC_DIR := $(DOC_DIR)/$(PROJECT)
TEXI_FILES      := $(TEXI_REF)

INFO_FILES     := $(TEXI_FILES:%.texi=$(PROJECT)-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)

INFO_REF       := $(PROJECT)-reference.info

all: info pdf html

webref: pdf-webref html-webref

all-formats: all dvi ps

install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_FILES) "$(LOCATION)/"
#	install -d -m 755 "$(LOCATION)/user"
#	install -m 644 user/* "$(LOCATION)/user/"
#	install -d -m 755 "$(LOCATION)/enduser"
#	install -m 644 enduser/* "$(LOCATION)/enduser/"

install: $(INFO_FILES)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)/"
	for i in $(INFO_FILES); do					\
	  install-info --info-dir="$(INFO_DIR)" $$i;			\
	done
	$(MAKE) install-at LOCATION="$(PROJECT_DOC_DIR)"
	-for i in $(DVI_FILES) $(PS_FILES); do				\
	  test -f $$i && install -m 644 $$i "$(PROJECT_DOC_DIR)";	\
	done
	-if test -f $(INFO_REF); then					\
	  install -m 644 $(INFO_REF)* "$(INFO_DIR)/" ;			\
	  install-info --info-dir="$(INFO_DIR)" $(INFO_REF) ;		\
	fi
	-if test -f reference.pdf ; then				\
	  install -m 644 reference.pdf "$(PROJECT_DOC_DIR)/" ;		\
	fi
	-if test -f reference ; then					\
	  install -d -m 755 "$(PROJECT_DOC_DIR)/reference" ;		\
	  install -m 644 reference/* "$(PROJECT_DOC_DIR)/reference/" ;	\
	fi
	-for i in reference.dvi reference.ps; do			\
	  test -f $$i && install -m 644 $$i "$(PROJECT_DOC_DIR)";	\
	done

uninstall:
	-rm -fr "$(PROJECT_DOC_DIR)"
	for i in $(INFO_FILES) $(INFO_REF); do				\
	  install-info --remove --info-dir="$(INFO_DIR)" $$i;		\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES) $(INFO_REF)*

info: $(INFO_FILES)

pdf: $(PDF_FILES) #splash.jpg
pdf-webref: webreference.pdf #splash.jpg

html: $(HTML_DIRS)
html-webref: webreference

dvi: $(DVI_FILES) #splash.eps

ps: $(PS_FILES) #splash.eps

SPLASH_INSERT := 's/^\@author/\@vskip 2cm\n\@image\{splash,16cm\}\n\@author/'

reference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl
#	$(PERL) -pi -e $(SPLASH_INSERT) $@

webreference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl --web
#	$(PERL) -pi -e $(SPLASH_INSERT) $@

$(PROJECT)-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) webreference: %: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.eps: %.jpg
	$(CONVERT) $< $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr declt.inc splash.eps webreference* $(TEXI_REF)		\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

install-www: webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	install -m 644 webreference.pdf "$(W3DIR)/reference.pdf"
	install -d -m 755 "$(W3DIR)/reference"
	install -m 644 webreference/* "$(W3DIR)/reference/"

declt.inc: $(TOP_DIR)/Makefile.cnf
	$(SBCL_PATH) $(SBCL_LOAD) ./declt.cl

.PHONY: all webref all-formats		\
	install-at install uninstall	\
	info				\
	pdf pdf-webref			\
	html html-webref		\
	dvi				\
	ps				\
	clean distclean			\
	update-version			\
	install-www


### Makefile ends here
