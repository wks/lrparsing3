#!/usr/bin/python
import sys
from lrparsing import *

class E(Grammar):
    e = Ref("e")
    binary_expr = Prio(e << Token("/") << e, e << Token("+") << e)
    unary_expr = Token("-") + e
    e = Prio(Token(re="[0-9]+"), unary_expr, binary_expr)
    START = e
    PRE_COMPILED = ""

print E.pre_compile_grammar(E.PRE_COMPILED)
