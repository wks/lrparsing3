#!/usr/bin/make -f
#
# (c) 2013, Russell Stuart.
# Licensed under GPLv2, or any later version.  See COPYING for details.

PREFIX	?= /usr
DOCDIR	= $(PREFIX)/share/doc/lrparsing

.PHONY: build
build:
	sphinx-build -n -b html -E . html
	rm -f html/index.html && ln -s lrparsing.html html/index.html

.PHONY: install install-doc
install: install-doc
install-doc:
	mkdir -p $(DESTDIR)$(DOCDIR)/html
	cp -a html/* $(DESTDIR)$(DOCDIR)/html/.
	mkdir -p $(DESTDIR)$(DOCDIR)/examples
	cp -a examples/* $(DESTDIR)$(DOCDIR)/examples/.
	mkdir -p $(DESTDIR)$(DOCDIR)/talk
	cp -a talk/* $(DESTDIR)$(DOCDIR)/talk/.

clean:
	rm -rf html .*.sw? *.py[co] */*.py[co]
