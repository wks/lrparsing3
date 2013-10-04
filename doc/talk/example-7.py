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


class Node(tuple):
    value = None
    def __new__(cls, n): return super(Node, cls).__new__(cls, n)
    def __repr__(self): return E.repr_parse_tree(self, False)


class Compiler(object):

    def __call__(self, node):
        node = Node(node)
        name = node[0].name
        if not isinstance(node[0], TokenSymbol):
            node.value = node[1].value
        else:
            name = name.split(".")[-1]
        if name in self.__class__.__dict__:
            self.__class__.__dict__[name](self, node)
        return node

    def number(self, node):
        node.value = int(node[1][1], 10)

    binary_ops = {'+': operator.add, '/': operator.floordiv}
    def binary_expr(self, node):
        node.value = self.binary_ops[node[2][1]](node[1].value, node[3].value)

    unary_ops = {'-': operator.neg}
    def unary_expr(self, node):
        node.value = self.unary_ops[node[1][1]](node[2].value)

print E.parse("2 + 3 / -1", tree_factory=Compiler()).value
