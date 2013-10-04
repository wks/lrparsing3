#!/usr/bin/python

from lrparsing import Grammar, Prio, THIS, Token

class Expr(Grammar):
    expr = Prio(
	'(' + THIS + ')' | Token(re="[0-9]+"),
	'+' + THIS | '-' + THIS,
	THIS + '**' + THIS,
	THIS + '/' + THIS,
	THIS + '+' + THIS)
    START = expr

print Expr.repr_parse_tree(Expr.parse("4 + -3 ** 2 / (1 ++ 2)"))
