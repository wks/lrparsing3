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

try:
    print E.repr_parse_tree(E.parse("""2 + 3 // -1"""))
except ParseError, e:
    sys.stderr.write("%s\n" % e)
