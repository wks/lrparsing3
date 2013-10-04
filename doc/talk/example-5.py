#!/usr/bin/python
from lrparsing import *
class Expr(Grammar):
    class T(TokenRegistry):
        a = Token('a'); b = Token('b')
    A = T.a; B = A + T.b
    START = A | B

parse_tree = Expr.parse("a b")
print Expr.repr_parse_tree(parse_tree, False)
print repr(parse_tree)
print str(Expr.START)
print repr(Expr.START)
