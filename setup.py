#!/usr/bin/python -W default
#
# (c) 2013, Russell Stuart.
# Licensed under GPLv2, or any later version.  See COPYING for details.
#
from distutils.core import setup
import re

def get_long_description():
  handle = open("doc/lrparsing.rst")
  while not next(handle).startswith("====="):
    pass
  long_description=[]
  for line in handle:
    if line.startswith("====="):
      break
    line = re.sub(":[a-z]*:`([^`<]*[^`< ])[^`]*`", "\\1", line)
    long_description.append(line)
  return ''.join(long_description[:-1])

setup(
    name="lrparsing",
    description="An LR(1) parser hiding behind a pythonic interface",
    long_description=get_long_description(),
    version="1.0.4",
    author="Russell Stuart",
    author_email="russell-lrparsing@stuart.id.au",
    url="http://www.stuart.id.au/russell/files/lrparsing",
    package_dir={"": "lrparsing"},
    py_modules=["lrparsing"],
    classifiers=[
	  "Development Status :: 4 - Beta",
	  "Intended Audience :: Developers",
	  "License :: OSI Approved :: GNU General Public License v2 or later (GPLv2+)",
	  "Natural Language :: English",
	  "Operating System :: OS Independent",
	  "Programming Language :: Python :: 2 :: Only",
	  "Topic :: Software Development :: Libraries :: Python Modules",
      ]
)
