#!/usr/bin/python
import operator
from lrparsing import *

class E(Grammar):
    e = Ref("e")
    number = Token(re="[0-9]+")
    binary_expr = Prio(e << Token("/") << e, e << Token("+") << e)
    unary_expr = Token("-") + e
    e = Prio(number, unary_expr, binary_expr)
    START = e

    @classmethod
    def eval_node(cls, n):
        return n[1] if not "eval" in n[0] else n[0]["eval"](n)

    binary_ops = {'+': operator.add, '/': operator.floordiv}
    unary_ops = {'-': operator.neg}
    number["eval"] = lambda n: int(n[1], 10)
    binary_expr["eval"] = lambda n: E.binary_ops[n[2]](n[1], n[3])
    unary_expr["eval"] = lambda n: E.unary_ops[n[1]](n[2])

print E.parse("2 + 3 / -1", tree_factory=E.eval_node)
