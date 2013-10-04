#!/usr/bin/make -f
#
# (c) Russell Stuart 2013.
# Licensed under GPLv2, or any later version.  See COPYING for details.

all:

.PHONY: install install-lrparsing
install: install-lrparsing
install-lrparsing:

.PHONY: clean
clean:
	rm -rf coverage .coverage *.py[co] .*.sw?

.PHONY: test
test:	
	mkdir -p coverage
	python-coverage run test.py
	rm -f coverage/*
	python-coverage html -d coverage
	@python-coverage report  --include lrparsing.py | grep --silent ' 100%$$' || { echo -n "Test coverage is not 100%: "; python-coverage report  --include lrparsing.py | grep '[0-9]\+%$$'; exit 1; }
