#!/usr/bin/python -W default
#
# A demo lrparsing.py - evaluating an expression.
#
# SYNOPSIS
# ========
#
#    lrparsing-expr.py 'expression' [...]
#
# (c) 2013, Russell Stuart.
# Licensed under GPLv2, or any later version.  See COPYING for details.
#
import operator
import sys

from lrparsing import Grammar, Prio, Ref, Token, Tokens, TokenRegistry

class ExprGrammar(Grammar):
    #
    # The Grammar.
    #
    class T(TokenRegistry):
        number = Token(re='[0-9]+')
        number["eval"] = lambda n: int(n[1], 10)

    expr = Ref("expr")
    minus_op = '-' >> expr
    brackets = '(' + expr + ')'
    pow_op = expr >> "**" >> expr
    mul_op = expr << Tokens('* / %') <<  expr
    add_op = expr << Tokens('+ -') <<  expr
    expr = T.number | brackets | Prio(pow_op, minus_op, mul_op, add_op)
    START = expr

    #
    # How it's evalualted.
    #
    @classmethod
    def eval_node(cls, n):
        return n[1] if not "eval" in n[0] else n[0]["eval"](n)

    BINARY_OP = {
            '+': operator.add,
            '-': operator.sub,
            '*': operator.mul,
            '/': operator.floordiv,
            '%': operator.mod,
            '**': operator.pow}
    eval_binary = lambda n: ExprGrammar.BINARY_OP[n[2]](n[1], n[3])
    UNARY_OP = {
            '-': operator.neg}
    eval_unary = lambda n: ExprGrammar.UNARY_OP[n[1]](n[2])
    minus_op["eval"] = eval_unary
    brackets["eval"] = lambda n: n[2]
    pow_op["eval"] = eval_binary
    mul_op["eval"] = eval_binary
    add_op["eval"] = eval_binary

#
# Entry point.
#
def main(argv=sys.argv):
    if len(argv) < 2:
        sys.stderr.write("usage: %s 'expression' [...]\n" % argv[0])
        sys.exit(1)
    for expr in argv[1:]:
        print "%s = %d" % (expr, ExprGrammar.parse(expr, ExprGrammar.eval_node))

if __name__ == "__main__":
    main()

# vim: set shiftwidth=4 expandtab softtabstop=8 :
