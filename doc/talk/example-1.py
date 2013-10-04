#!/usr/bin/python

from lrparsing import Grammar, THIS, Token

class Expr(Grammar):
    expr = (
	THIS + '/' + THIS |
	THIS + '+' + THIS |
	THIS + '**' + THIS |
	'+' + THIS |
	'-' + THIS |
	'(' + THIS + ')' |
	Token(re="[0-9]+"))
    START = expr

print Expr.repr_parse_tree(Expr.parse("4 + -3 ** 2 / (1 ++ 2)"))
