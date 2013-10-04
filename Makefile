#!/usr/bin/make -f
#
# (c) 2013, Russell Stuart.
# Licensed under GPLv2, or any later version.  See COPYING for details.

ME=$(shell basename $$PWD)
DEBIANPACKAGE=$(shell echo "$(ME)" | sed -e 's/-[^-]*$$//')
PACKAGE=$(shell echo "$(ME)" | sed -e 's/-[^-]*$$//' -e 's/-/_/')
VERSION=$(shell echo "$(ME)" | sed 's/.*-//')
YEAR=$(shell date +%Y)
MONTH=$(shell date +%b)
DATE=$(shell date +%Y-%m-%d)

DESTDIR ?= /

.PHONY:	all
all:	doc lrparsing

.PHONY:	lrparsing
lrparsing:
	$(MAKE) --directory lrparsing

.PHONY:	doc
doc:
	$(MAKE) --directory doc

.PHONY:	test
test:	
	$(MAKE) --directory lrparsing $@

.PHONY:	clean
clean:
	$(MAKE) --directory lrparsing $@
	$(MAKE) --directory doc $@
	rm -rf release build dist MANIFEST .*.sw?

.PHONY:	install
install: install-doc install-lrparsing

.PHONY:	install-doc
install-doc:
	$(MAKE) --directory doc $@

.PHONY:	install-lrparsing
install-lrparsing:
	$(MAKE) --directory lrparsing $@
	python setup.py install $(DIST_PYFLAGS) --root=$(DESTDIR)

.PHONY: pypi
pypi:
	python setup.py register sdist bdist upload --sign --identity=0xE7843A8C

.PHONY:	release
release:	clean
	echo ME=$(ME) PACKAGE=$(PACKAGE) VERSION=$(VERSION) YEAR=$(YEAR) MONTH=$(MONTH) DATE=$(DATE)
	#
	# Update all the version numbers and dates.
	#
	sed -i '/$(YEAR)/!s/^\( \+Copyright (c) .*2[0-9]*\)\(,[ ]*Russell Stuart\)/\1,$(YEAR)\2/' README.txt
	sed -i '/$(YEAR)/!s/^\(# .*Copyright (c) .*2[0-9]*\)\([ ]*Russell Stuart\)/\1,$(YEAR)\2/' */*.py */Makefile */*.rst
	sed -i '/$(YEAR)/!s/^\( *Copyright (c) .*2[0-9]*\)\([ ]*Russell Stuart\)/\1,$(YEAR)\2/' */*.rst
	sed -i '/$(YEAR)/!s/\(.* is copyright &copy; .*2[0-9]*\)\([ ]*Russell Stuart\)/\1,$(YEAR)\2/' $(PACKAGE).html
	sed -i 's/$(PACKAGE)-[0-9]\+[.][0-9.]\+\([-.]\)/$(ME)\1/g' $(PACKAGE).html
	sed -i 's/^\([ 	]*\(version\|release\)="\)[0-9]\+[.][0-9.]\+/\1$(VERSION)/' setup.py
	sed -i 's/^\([ 	]*\(version\|release\) *= *'\''\)[0-9]\+[.][0-9.]\+/\1$(VERSION)/' doc/conf.py
	#sed -i 's/^\(Version:[ 	]*\)[0-9]\+[.][0-9]\+/\1$(VERSION)/g' $(PACKAGE).spec
	#
	# Build the www directory.
	#
	mkdir -p release/$(PACKAGE)/doc
	(cd ..; tar cfz $(ME)/release/$(PACKAGE)/$(ME).tar.gz --exclude=$(ME)/debian --exclude=$(ME)/release $(ME))
	cp -a ChangeLog.txt README.txt COPYING index.html $(PACKAGE).html release/$(PACKAGE)
	cp release/$(PACKAGE)/$(ME).tar.gz ../$(PACKAGE)_$(VERSION).orig.tar.gz
	$(MAKE) doc
	cp -a doc/html doc/examples doc/talk release/$(PACKAGE)/doc
	ln -s html/index.html release/$(PACKAGE)/doc/index.html
	rm release/$(PACKAGE)/doc/examples/lrparsing.py
	#
	# Build the RPM package.
	#
	#mkdir -p release/rpm/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	#echo >release/rpm/rpmmacros "%_topdir $(PWD)/release/rpm"
	#TAR_OPTIONS=--wildcards rpmbuild -ta --macros "/usr/lib/rpm/macros:/usr/lib/rpm/platform/$(RPM_ARCH)-linux/macros:release/rpm/rpmmacros" "release/$(PACKAGE)/$(ME).tar.gz"
	#mv "release/rpm/SRPMS/$(ME)-1ras.src.rpm" "release/$(PACKAGE)/."
	#mv "release/rpm/RPMS"/*/"$(ME)-1ras.noarch.rpm" "release/$(PACKAGE)/."
	#rm -r "release/rpm"
