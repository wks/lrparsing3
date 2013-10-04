#!/usr/bin/python
import sys
from lrparsing import *

class E(Grammar):
    class T(TokenRegistry):
        number = Token(re="[0-9]+")
    e = Ref("e")
    binary_expr = Prio(e << Token("/") << e, e << Token("+") << e)
    unary_expr = Token("-") + e
    e = Prio(T.number, unary_expr, binary_expr)
    START = e

def on_error(iterator, token, stack):
    for item in stack:
        print "([%s], (%s))" % (
            ','.join(sorted(str(rule) for rule in item[0].rules)),
            ','.join(
                E.repr_parse_tree(sym, indent="")
	    	for sym in item[1]))

print E.repr_parse_tree(E.parse("""2 + 3 / -1""", on_error=on_error))
