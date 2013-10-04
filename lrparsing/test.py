#!/usr/bin/python -W default
#
# lrparsing.py has 100% test coverage.  This is it.
#
# To run:
#
#   mkdir -p coverage && \
#       python-coverage run lrparsing-test.py && \
#       rm -f coverage/* && \
#       python-coverage html -d coverage && \
#       firefox `pwd`/coverage/index.html &
#
# It should say there is 100% coverage for lrparsing.py ...
#
# (c) 2013, Russell Stuart.
# Licensed under GPLv2, or any later version.  See COPYING for details.
#
import re
import sys

import lrparsing
from lrparsing import (
        Assoc, Choice, Grammar, Keyword, Left, List, Many, Nonassoc, Opt, Prio,
        Repeat, Sequence, Some, Ref, Right, THIS, Token, TokenRegistry, Tokens,
        UnrecognisedToken, UserToken)


def main(argv=sys.argv):
    test_repr()
    test_compile_symbol()
    test_dict()
    test_build_lr1_table()
    test_pre_compile()
    test_tokenisation()
    test_lr1_parser()
    test_grammar_errors()
    test_parsing_error()
    test_notimplemented()


def test_repr():
    #
    # Every symbol's repr.
    #
    i = 0
    for rule, repr_start  in test_repr_cases:
        grammar = type('G', (Grammar,), {'START': rule})
        rhs = repr(grammar.START).split(" = ", 1)[1]
        assert rhs == repr_start, (i, rhs)
        i += 1
    #
    # Hard to get to special cases.
    #
    assert repr(Ref("x")) == "Ref('x')", repr(Ref("x"))
    class G(Grammar):
        class T(TokenRegistry):
            t = Token("x")
        START = (T.t << T.t) + T.t
    assert repr(G.T.t) == "T.t='x'", repr(G.T.t)
    assert repr(G.START) == "START = (T.t='x' << T.t='x') + T.t='x'", repr(G.START)
    #
    # First grammar printing.
    #
    class G(Grammar):
        START = Some(Token("x") | Token("y"))
        result_repr_grammar = "<G> = START + __end_of_input__\nSTART = ('x' | 'y') * Some"
        result_repr_productions = "0     : <G> = START __end_of_input__\n1     : START = START.Some\n2     : START.Some = 'x'\n        START.Some = 'y'\n        START.Some = START.Some 'x'\n        START.Some = START.Some 'y'"
        result_repr_parse_table = "ItemSet:0\n  <G> = ^ START __end_of_input__ [__empty__] {():[]}\n  -- closure\n    START = ^ START.Some [__end_of_input__] {():[__end_of_input__]}\n    START.Some = ^ 'x' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n    START.Some = ^ 'y' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n    START.Some = ^ START.Some 'x' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n    START.Some = ^ START.Some 'y' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': shift ItemSet:3\n    'y': shift ItemSet:4\n  -- gotos\n    START: ItemSet:1\n    START.Some: ItemSet:2\n\nItemSet:1\n  <G> = START ^ __end_of_input__ [__empty__] {():[]}\n  -- actions\n    __end_of_input__: shift ItemSet:5\n\nItemSet:2\n  START = START.Some ^ [__end_of_input__] {():[__end_of_input__]}\n  START.Some = START.Some ^ 'x' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  START.Some = START.Some ^ 'y' ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': shift ItemSet:6\n    'y': shift ItemSet:7\n    __end_of_input__: reduce START = START.Some\n\nItemSet:3\n  START.Some = 'x' ^ ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': reduce START.Some = 'x'\n    'y': reduce START.Some = 'x'\n    __end_of_input__: reduce START.Some = 'x'\n\nItemSet:4\n  START.Some = 'y' ^ ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': reduce START.Some = 'y'\n    'y': reduce START.Some = 'y'\n    __end_of_input__: reduce START.Some = 'y'\n\nItemSet:5\n  <G> = START __end_of_input__ ^ [__empty__] {():[]}\n  -- actions\n    __empty__: reduce <G> = START __end_of_input__\n\nItemSet:6\n  START.Some = START.Some 'x' ^ ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': reduce START.Some = START.Some 'x'\n    'y': reduce START.Some = START.Some 'x'\n    __end_of_input__: reduce START.Some = START.Some 'x'\n\nItemSet:7\n  START.Some = START.Some 'y' ^ ['x','y',__end_of_input__] {():['x','y',__end_of_input__]}\n  -- actions\n    'x': reduce START.Some = START.Some 'y'\n    'y': reduce START.Some = START.Some 'y'\n    __end_of_input__: reduce START.Some = START.Some 'y'"
        result_repr_parse_table_pre = "Lr1State:0\n  -- actions\n    'x': shift 3\n    'y': shift 4\n  -- gotos\n    2: 1\n    4: 2\n\nLr1State:1\n  -- actions\n    __end_of_input__: shift 5\n\nLr1State:2\n  -- actions\n    'x': shift 6\n    'y': shift 7\n    __end_of_input__: reduce 2 1 START\n\nLr1State:3\n  -- actions\n    'x': reduce 4 1\n    'y': reduce 4 1\n    __end_of_input__: reduce 4 1\n\nLr1State:4\n  -- actions\n    'x': reduce 4 1\n    'y': reduce 4 1\n    __end_of_input__: reduce 4 1\n\nLr1State:5\n  -- actions\n    __empty__: reduce 0 2 <G>\n\nLr1State:6\n  -- actions\n    'x': reduce 4 2\n    'y': reduce 4 2\n    __end_of_input__: reduce 4 2\n\nLr1State:7\n  -- actions\n    'x': reduce 4 2\n    'y': reduce 4 2\n    __end_of_input__: reduce 4 2"
        result_repr_parse_table_pre_0 = "Lr1State:0\n  -- actions\n    'x': shift 3\n    'y': shift 4\n  -- gotos\n    2: 1\n    4: 2"
        result_repr_parse_table_pre_1 = "Lr1State:0\n  -- actions\n    'x': shift 3\n    'y': shift 4\n  -- gotos\n    2: 1\n    4: 2\n\nLr1State:1\n  -- actions\n    __end_of_input__: shift 5"
    assert G.repr_grammar() == G.result_repr_grammar, repr(G.repr_grammar())
    G.compile_grammar()
    assert G.repr_productions() == G.result_repr_productions, repr(G.repr_productions())
    assert G.repr_parse_table() == G.result_repr_parse_table, repr(G.repr_parse_table())
    G.pre_compile_grammar()
    assert G.repr_parse_table() == G.result_repr_parse_table_pre, repr(G.repr_parse_table())
    assert G.repr_parse_table(0) == G.result_repr_parse_table_pre_0, repr(G.repr_parse_table(0))
    assert G.repr_parse_table(-1) == G.result_repr_parse_table_pre_1, repr(G.repr_parse_table(-1))
    #
    # Special grammar for testing repr_parse_tree().
    #
    class G(Grammar):
        z = Token("z")
        u = Opt(Token("u"))
        w1 = Token("w") + z
        w = z + Opt(w1)
        y = Token("y") + w
        x = Token("x") + u + y
        START = x
        result_repr_parse_tree_1 = "(START (x 'x' (u) (y\n  'y'\n  (w\n    (z 'z')\n    (w1 'w' (z 'z')))))"
        result_repr_parse_tree_2 = "(START (x 'x' (u) (y 'y' (w (z 'z') (w1 'w' (z 'z')))))"
    assert G.repr_parse_table() == '', G.repr_parse_table()
    parse_tree = G.parse("xyzwz")
    result = G.repr_parse_tree(parse_tree)
    assert result == G.result_repr_parse_tree_1, repr(result)
    result = G.repr_parse_tree(parse_tree, False)
    assert result == G.result_repr_parse_tree_2, repr(result)
    result = G.repr_parse_tree(parse_tree[1][1])
    assert result == "'x'", result
    #
    # ItemSet.__repr__ when actions are sets, which only happens if the compile
    # fails.
    #
    class G(Grammar):
        x = Token("x")
        START = Token("x") | x
        repr_parse_table_result = "ItemSet:0\n  <G> = ^ START __end_of_input__ [__empty__] {():[]}\n  -- closure\n    START = ^ 'x' [__end_of_input__] {():[__end_of_input__]}\n    START = ^ x [__end_of_input__] {():[__end_of_input__]}\n    x = ^ 'x' [__end_of_input__] {():[__end_of_input__]}\n  -- actions\n    'x': shift ItemSet:2\n  -- gotos\n    START: ItemSet:1\n    x: ItemSet:3\n\nItemSet:1\n  <G> = START ^ __end_of_input__ [__empty__] {():[]}\n  -- actions\n    __end_of_input__: shift ItemSet:4\n\nItemSet:2\n  START = 'x' ^ [__end_of_input__] {():[__end_of_input__]}\n  x = 'x' ^ [__end_of_input__] {():[__end_of_input__]}\n  -- actions\n    __end_of_input__: reduce START = 'x', reduce x = 'x'\n\nItemSet:3\n  START = x ^ [__end_of_input__] {():[__end_of_input__]}\n  -- actions\n    __end_of_input__: reduce START = x\n\nItemSet:4\n  <G> = START __end_of_input__ ^ [__empty__] {():[]}\n  -- actions\n    __empty__: reduce <G> = START __end_of_input__"
    try:
        G.compile_grammar()
        assert False, "GrammarError expected because Grammar is ambiguous"
    except lrparsing.GrammarError, e:
        pass
    result = G.repr_parse_table()
    assert result == G.repr_parse_table_result, repr(result)
    #
    # Check unused_rules.
    #
    class G(Grammar):
        unused = Token("+")
        START = Token("x")
    G.compile_grammar()
    assert G.unused_rules() == frozenset((G.unused,)), G.unused_rules() 
    #
    # Bit hard to invoke self.parent is None in Symbol.__str__, as it is
    # there purely for internal debugging.  Still, 100% test coverage is
    # 100% required.
    #
    str(lrparsing.Symbol())
    #
    # TokenSymbol.position()
    #
    class G(Grammar):
        class T(TokenRegistry):
            a = UserToken()
        START = Token("x")
    try:
        G.parse(((G.T.a,),))
    except lrparsing.ParseError:
        pass
    #
    # Token.position() for non-standard tuples.
    #
    class G(Grammar):
        class T(TokenRegistry):
            x = Token('x')
        START = Token("y")
    try:
        G.parse(((G.T.x,),))
    except lrparsing.ParseError:
        pass
    #
    # And epoch_symbol()
    #
    class G(Grammar):
        START = Token("x")
    assert str(G.epoch_symbol()) == "<G>", repr(str(G.epoch_symbol()))


test_repr_cases = [
        (THIS,                          'START'),
        (Token('x'),                    "'x'"),
        (Token('x') + Token('y'),       "'x' + 'y'"),
        (Token('x') | Token('y'),       "'x' | 'y'"),
        (Prio('x'),                     "Prio(Prioritised(\'x\'))"),
        (Prio('x', "y"),                "(Prioritised('x'), Prioritised('y'))"),
        (Assoc('n', "x"),               "Assoc(n, 'x')"),
        (Left("x"),                     "Left('x')"),
        (Right("x"),                    "Right('x')"),
        (Nonassoc("x"),                 "Nonassoc('x')"),
        (List("x", ","),                "List('x', ',')"),
        (List("x", ",", 1),             "List('x', ',', 1)"),
        (List("x", ",", 1, 2),          "List('x', ',', 1, 2)"),
        (List("x", ",", 1, 2, True),    "List('x', ',', 1, 2, True)"),
        (Repeat(","),                   "',' * ()"),
        (Repeat(",", 1),                "',' * (1,)"),
        (Repeat(",", 1, 1),             "',' * 1"),
        (Repeat(",", 1, 2),             "',' * (1, 2)"),
        (Repeat(Token(",") + Token("x")),"(',' + 'x') * ()"),
        (Token(",") * Opt,              "',' * Opt"),
        (Token(",") * Some,             "',' * Some"),
        (Token(",") * Many,             "',' * Many"),
        (UserToken("x"),                "x"),
        (UnrecognisedToken(),           "UnrecognisedToken()"),
        (Token(re="x"),                 "/x/"),
]


#
# Test the operation of a symbol's mapping interface.
#
def test_dict():
    class G(Grammar):
        class T(TokenRegistry):
            t = Token("t")
            t["t"] = "z"
        z = Token("a")
        z["b"] = "q"
        z["c"] = "c"
        y = z | Token("a") + Token("b")
        y["y"] = "yy"
        START = y
    assert G.T.t["t"] == "z", G.T.t["z"]
    assert G.T.t.dict == {"t": "z"}, G.T.t.dict
    assert 'c' in G.z, G.z.dict
    del G.z['c']
    assert G.z.dict == {"b": "q"}, G.z.dict
    assert G.y.dict == {"y": "yy"}, (G.y.dict, G.START.dict)
    assert G.START.dict == {}
    #
    # Test the case where "START" if discovered 2nd.
    #
    class G(Grammar):
        A0 = Token("A0")
        A0["a"] = "b"
        START = A0
    assert G.A0.dict == {"a": "b"}, G.A0.dict
    assert G.START.dict == {}, G.START.dict
    assert G.START, bool(G.START)


#
# Test the process or turning the grammar tree into productions:
#
#   Symbol.compile_symbol()
#   Symbol.resolve_symbol()
#
def test_compile_symbol():
    #
    # Ensure every Symbol.resolve_symbol and Symbol.compile_symbol is
    # run.
    #
    i = 0
    for rule, repr_productions  in test_compile_symbol_cases:
        G = type('G', (Grammar,), {'START': rule})
        try:
            G.compile_grammar()
            result = G.repr_productions()
            result = result[result.find("\n") + 1:]
            assert result == repr_productions, (i, result)
        except:
            print G.repr_grammar()
            print G.repr_productions()
            print G.repr_parse_table()
            raise
        i += 1
    #
    # Ensure every path in Parser.catalogue_symbols() is run.
    #
    class G(Grammar): x = Token("x"); START = x
    class G(Grammar): A0 = Token("x"); START = A0
    class G(Grammar):
        class T(TokenRegistry): t = Token("x")
        START = T.t
    #
    # Ensure every path in Parser.resolve_lhs gets run.
    #
    #class G(Grammar): y = Ref("START"); START = y + Token("x")
    #
    # Exercise comments.
    #
    class G(Grammar): START = Token("x"); COMMENTS = Token("#")
    class G(Grammar): START = Token("x"); COMMENTS = Token("#") | Token("//")

test_compile_symbol_cases = [
    (   Token('x'),
        "1     : START = 'x'"),
    (   Sequence(Token('x'), Token("y")),
        "1     : START = 'x' 'y'"),
    (   Choice(Token('x'), Token("y")),
        "1     : START = 'x'\n        START = 'y'"),
    (   Token('x') | Token("y"),
        "1     : START = 'x'\n        START = 'y'"),
    (   Token('x') + Token("y"),
        "1     : START = 'x' 'y'"),
    (   Token('x') + Token("y") + Token('z'),
        "1     : START = 'x' 'y' 'z'"),
    (   (Token('x') + Token("y")) + (Token('z') + Token('a')),
        "1     : START = 'x' 'y' 'z' 'a'"),
    (   Left(Token('x')),
        "1     : START = START.Left\n2     : START.Left = 'x'"),
    (   Token('x') << Token('y'),
        "1     : START = START.Left\n2     : START.Left = 'x' 'y'"),
    (   Right(Token('x')),
        "1     : START = START.Right\n2     : START.Right = 'x'"),
    (   Token('x') >> Token('y'),
        "1     : START = START.Right\n2     : START.Right = 'x' 'y'"),
    (   Prio(Token("x")),
        "1     : START = START.Prio\n2     : START.Prio = START.Prio.Prioritised\n3.1   : START.Prio.Prioritised = 'x'"),
    (   Prio(Prio(Token("x"))),
        "1     : START = START.Prio\n2     : START.Prio = START.Prio.Prioritised\n3.1   : START.Prio.Prioritised = 'x'"),
    (   List("x", ",", 1, 2),
        "1     : START = 'x'\n        START = 'x' ',' 'x'"),
    (   List("x", ",", 0, 2),
        "1     : START = \n        START = 'x'\n        START = 'x' ',' 'x'"),
    (   List("x", ",", 0, 1, True),
        "1     : START = \n        START = 'x'\n        START = 'x' ','"),
    (   List("x", ","),
        "1     : START = \n        START = START.List\n2     : START.List = 'x'\n        START.List = START.List ',' 'x'"),
    (   List("x", ",", 1),
        "1     : START = START.List\n2     : START.List = 'x'\n        START.List = START.List ',' 'x'"),
    (   List("x", ",", 1, None, True),
        "1     : START = START.List\n        START = START.List ','\n2     : START.List = 'x'\n        START.List = START.List ',' 'x'"),
    (   Right(List("x", ",")),
        "1     : START = START.Right\n2     : START.Right = \n        START.Right = START.Right.List\n3     : START.Right.List = 'x'\n        START.Right.List = 'x' ',' START.Right.List"),
    (   Right(List("x", ",", opt=True)),
        "1     : START = START.Right\n2     : START.Right = \n        START.Right = START.Right.List\n3     : START.Right.List = 'x'\n        START.Right.List = 'x' ',' START.Right.List\n        START.Right.List = 'x' ','"),
    (   Repeat("."),
        "1     : START = \n        START = START.Repeat\n2     : START.Repeat = '.'\n        START.Repeat = START.Repeat '.'"),
    (   Repeat(".", 0, 1),
        "1     : START = \n        START = '.'"),
    (   Right(Repeat(".")),
        "1     : START = START.Right\n2     : START.Right = \n        START.Right = START.Right.Repeat\n3     : START.Right.Repeat = '.'\n        START.Right.Repeat = '.' START.Right.Repeat"),
    (   Token(re="k..") | Tokens("a b", "kwd"),
        "1     : START = /k../\n        START = 'a'\n        START = 'b'\n        START = 'kwd'"),
    (   Token("x") | Token("x") + Token("x"),
        "1     : START = 'x'\n        START = 'x' 'x'"),
    (   Token("x") * 2,
        "1     : START = 'x' 'x'"),
    (   Token("x") * (2,3),
        "1     : START = 'x' 'x'\n        START = 'x' 'x' 'x'"),
    (   Ref("START") + Token("x"),
        "1     : START = START 'x'"),
    (   THIS + Token("x"),
        "1     : START = START 'x'"),
    (   "x" + THIS,
        "1     : START = 'x' START"),
    (   "x" | THIS,
        "1     : START = 'x'\n        START = START"),
    (   1 * THIS,
        "1     : START = START"),
    (   (0, 1) * THIS + 'x',
        "1     : START = 'x'\n        START = START 'x'"),
    (   Opt * THIS,
        "1     : START = \n        START = START"),
    (   "x" << THIS,
        "1     : START = START.Left\n2     : START.Left = 'x' START"),
    (   "x" >> THIS,
        "1     : START = START.Right\n2     : START.Right = 'x' START"),
]


#
# Test building the parser table.
#
def test_build_lr1_table():
    #
    # Exercise all of Nonterm.merge_first_set() and Parser.calc_first_sets().
    #
    class TestMergeFirstSet(Grammar):
        epsilon = Opt("x")
        START = epsilon + "y"
    TestMergeFirstSet.compile_grammar()
    #
    # Exercise all of ItemSet._close_kernel_items(), ItemSet.merge(),
    # and ItemSet.compatible().
    #
    class TestCloseKernelItems(Grammar):
        c = Token("c")
        d = Token("d") + c
        START = Prio(
                Token("n") | Token("(") + THIS + ")",
                THIS >> Tokens("+ -"),
                Token("z") + c) | d
    TestCloseKernelItems.compile_grammar()
    #
    # Exercise off of Parser.disambiguate() and Parser.resolve_ambiguity().
    #
    class TestDisambiguate(Grammar):
        a = Token("a") | Nonassoc(THIS + "<" + THIS)
        START = Prio(
                "n",
                THIS << "+" << THIS,
                Token("*") >> Right(Repeat(THIS)),
                a)
    try:
        TestDisambiguate.compile_grammar()
    except lrparsing.GrammarError, e:
        print TestDisambiguate.repr_grammar()
        print TestDisambiguate.repr_productions()
        print TestDisambiguate.repr_parse_table()
        raise
    #
    # Exercise ShiftAction.precedence().
    #
    class TestPrecedence(Grammar):
        call = Token('n') + '.'  + 'x'
        field = Token('n') + '.' + 'n'
        n = Token('n')
        START = Prio(call, field, n) + '.'
    TestPrecedence.compile_grammar()
    #
    # For some reason ReduceAction.__eq__ isn't called.  I don't have a
    # clue why.
    #
    cache = {}
    prod = TestPrecedence.START.productions[0]
    lr1_item_a = lrparsing.Lr1Item(lrparsing.Lr0Item(prod, 0, cache), ())
    lr1_item_b = lrparsing.Lr1Item(lrparsing.Lr0Item(prod, 1, cache), ())
    reduce_1 = lrparsing.ReduceAction(lr1_item_a)
    reduce_2 = lrparsing.ReduceAction(lr1_item_a)
    reduce_3 = lrparsing.ReduceAction(lr1_item_b)
    assert reduce_1 == reduce_2
    assert reduce_1 != reduce_3


#
# Exercise pre-compile.
#
def test_pre_compile():
    class G(Grammar):
        START = Repeat(Token("x"))
    result = G.pre_compile_grammar()
    G.compile_grammar()                 # Flush pre-compile
    assert G.pre_compile_grammar(result) is None
    assert G.pre_compile_grammar(result) is None
    G.compile_grammar()                 # Flush pre-compile
    assert G.pre_compile_grammar(eval(result)) is None


#
# Test Tokeniser.
#
def test_tokenisation():
    #
    # Breaking up tokens and tuples.
    #
    class G(Grammar):
        class T(TokenRegistry):
            user = UserToken()
        START = (
                Token("x") + Token("xx") + Token("x") + T.user +
                Token("y y") + Token("x") + T.user)
    parse_tree = G.parse(
            ("x\nx", "x\nx\n", (G.T.user,), "y ", "y", "x", (G.T.user,),))
    result = G.repr_parse_tree(parse_tree)
    assert result == "(START 'x' 'xx' 'x' T.user 'y y' 'x' T.user)", result
    #
    # Keywords, re's and no whitespace.
    #
    class G(Grammar):
        class T(TokenRegistry):
            token9 = UserToken()
            def match_token9(registry, data):
                return registry.token9 if data == "9" else registry.n
            n = Token(re="[0-9]+", refine=match_token9)
            w = Token(re="[b-zB-Z]+")
            z0 = Token(re="\\A_[a-z]")
            z1 = Token("_a")
        WHITESPACE = ""
        START = (
                Keyword("1") + "," + T.token9 + "," + T.n +
                Token("a", case=False) + "," + T.w + "," + Keyword("b"))
    parse_tree = G.parse("1,9,0A,B,b")
    result = G.repr_parse_tree(parse_tree)
    assert result == "(START '1' ',' T.token9 ',' '0' 'A' ',' 'B' ',' 'b')", result
    #
    # The UnrecognisedToken().
    #
    class G(Grammar):
        START = UnrecognisedToken()
    parse_tree = G.parse("x")
    result = G.repr_parse_tree(parse_tree)
    assert result == "(START 'x')"
    #
    # Captures and back references in tokenisation.
    #
    class G(Grammar):
        START = Repeat(
                Token(re="(?:(x))") |
                Token(re="a(b*),\\1c") |
                Token(re="d(?:(e))(y*);\\2"))
        repr_parse_tree_result = "(START 'x' 'x' 'abb,bbc' 'deyyyy;yyyy' 'abb,bbc')"
    result = G.repr_parse_tree(G.parse("x x abb,bbc deyyyy;yyyy abb,bbc"))
    assert result == G.repr_parse_tree_result, repr(result)


#
# Exercise the LR(1) parser.
#
def test_lr1_parser():
    #
    # A successful parse.
    #
    class G(Grammar):
        COMMENTS = Token(",")
        empty = Opt(Token("e"))
        START = Token("x") + empty
    def log(line):
        log_lines.append(line)
    log_lines = []
    MARK = "i was here!"
    def tree_factory(nodes):
        return nodes + (MARK,)
    parse_tree = G.parse(",x,", tree_factory, None, log)
    assert parse_tree[2][1] == MARK and parse_tree[3] == MARK
    result = '\n'.join(log_lines)
    assert result == "shift  'x'; ItemSet:2='x'\nreduce __end_of_input__;  -- ItemSet:4=empty ItemSet:2='x'\nreduce __end_of_input__; ItemSet:4=empty ItemSet:2='x' -- ItemSet:1=START"
    #
    # Error recovery.
    #
    class G(Grammar):
        class T(TokenRegistry):
            x = Token("x")
            y = Token("y")
        START = T.x + T.y
    def on_error(iterator, token, stack):
        if token[1] == 'y':
            return [(G.T.x, 'x') + token[2:]]
        return None
    try:
        G.parse("yx", on_error=on_error)
        assert False
    except lrparsing.ParseError, e:
        assert 'line 1 column 2: Got T.x when expecting T.y while trying to match START in state 2' in str(e), repr(str(e))


#
# Test parsing errors.
#
def test_parsing_error():
    class G(Grammar): START = Token('x')
    try:
        G.parse("y")
        assert False
    except lrparsing.TokenError, e:
        pass
    try:
        G.parse("xx")
        assert False
    except lrparsing.ParseError, e:
        pass
    class G0(Grammar): START = Token("a") + Tokens("b c d e f g h i j k l m n")
    try:
        G0.parse("aa")
        assert False
    except lrparsing.ParseError, e:
        pass
    class G1(Grammar): START = Token("a") + Tokens("d e")
    try:
        G1.parse("aa")
        assert False
    except lrparsing.ParseError, e:
        pass
    class G2(Grammar): START = Token("x")
    try:
        G2.parse("")
        assert False
    except lrparsing.ParseError, e:
        pass


#
# Test for grammar errors.
#
def test_grammar_errors():
    i = 0
    for error_re, grammar_dict  in test_grammar_error_tests:
        try:
            dct = grammar_dict() if callable(grammar_dict) else grammar_dict
            grammar = type('G', (Grammar,), dct)
            grammar.compile_grammar()
            assert False, (i, error_re)
        except lrparsing.GrammarError, e:
            match = re.search('(?is)' + error_re, str(e).lower())
            assert match is not None, (i, error_re, str(e))
        i += 1


def registry(name, *args, **kwds):
    dct = {}
    if args:
        dct.update(args[0])
    dct.update(kwds)
    return type(name, (TokenRegistry,), dct)


test_grammar_error_tests = [
    (   "token and symbol share the same name",
        {'__end_of_input__': Token("+")},),
    (   "No START symbol defined",
        {'x': Token("+")},),
    (   "START is not a Nonterm",
        {'START': 0},),
    (   "is reserved",
        {'<G>': Token('x'), 'START': Token('x')}),
    (   "COMMENTS must be Token",
        {'COMMENTS': 0, 'START': Token('x')}),
    (   "assigned directly",
        lambda: (lambda x: {'x': x, 'y': x, 'START': x})(Token('x')),),
    (   "have more than one",
        {'X': registry('X'), 'Y': registry('Y'), 'START': Token('z')}),
    (   "Reduce/Reduce",
        {'x': Token('x'), 'y': Token('x'), 'START': Ref('x') | Ref('y')},),
    (   "Shift/Reduce",
        {'START': Token('s') | THIS + Token('+') + THIS},),
    (   "Shift/Reduce",
        {'START': Token('s') | Left(THIS + Token('+') + THIS) | Right(THIS + Token('-') + THIS)},),
    (   "right operand of *",
        lambda: {'START': Token("s") * "x"}),
    (   "can't be a Symbol",
        lambda: {'START': Token("s") + 9}),
    (   "hasn't been defined",
        {'START': Ref("s")}),
    (   "references undefined",
        {'START': Ref("s") * 1}),
    (   "Can not import",
        {'START': type("G1", (Grammar,), {'START': Token('x')}).START},),
    (   "can not be used",
        {'START': lrparsing.MetaToken("x")}),
    (   "Unknown associativity",
        lambda: {'START': lrparsing.Assoc('.')}),
    (   "associativity on",
        {'START': Nonassoc(Repeat(Token('x')))}),
    (   "min may not be greater than max",
        lambda: {'START': Repeat(Token('x'), 2, 1)}),
    (   "associativity on",
        {'START': Nonassoc(List(Token('x'), Token("y")))},),
    (   "min may not be greater than max",
        lambda: {'START': List(Token('x'), Token("y"), 2, 1)},),
    (   "WHITESPACE must be a string",
        {'START': Token('x'), 'WHITESPACE': 1}),
    (   "define the same re",
        lambda: (lambda T: {'T': T, "START": T.n1 + T.n2})(registry('T', n1=Token(re="x"), n2=Token(re="x"))),),
    (   "duplicate literal",
        lambda: (lambda T: {'T': T, "START": T.n1 + T.n2})(registry('T', n1=Token("x"), n2=Token("x"))),),
    (   "does not match any re",
        {"START": Keyword("x")}),
    (   "Literal token '.*' should be a Keyword",
        {"START": Token("x") | Token(re="x")}),
    (   "partially matches",
        {"START": Keyword("xy") | Token(re="x")},),
    (   "doesn't support merging",
        {"START": UserToken("'n'") + Token('n')},),
    (   "can not rename",
        {'T': registry('T', t=UserToken('x')), "START": Token('x')}),
    (   'A UserToken must be assigned a name using a TokenRegistry',
        {"START": UserToken()}),
    (   "A keyword must not have an re",
        lambda: {"START": Token(re="x", kind=Token.KEYWORD)}),
    (   "A keyword can not be refined",
        lambda: {"START": Token("x", refine="x", kind=Token.KEYWORD)}),
    (   "A keyword must have a literal",
        lambda: {"START": Token(kind=Token.KEYWORD)}),
    (   "UnrecognisedToken can't have a literal or re",
        lambda: {"START": Token("x", kind=Token.UNRECOGNISED)}),
    (   "Unrecognised Token kind",
        lambda: {"START": Token(kind=object())}),
    (   "must have a literal or an re",
        lambda: {"START": Token()}),
    (   "Token can't have both a literal and a re",
        lambda: {"START": Token("x", "x")}),
    (   "A literal can't be refined",
        lambda: {"START": Token("x", refine="x")}),
    (   "doesn't support merging",
        lambda: (lambda T: {'T': T, "START": T.t | UserToken('T.t')})(registry('T', t=Token('t')))),
    (   "Can not rename token",
        lambda: (lambda T: {'T': T, "START": T.x | T.y})((lambda t: registry('T', {'x': t, 'y': t}))(Token("x")))),
    (   "defined with conflicting refine's",
        {'START': Token(re="x", refine="x") | Token(re="x", refine="y")}),
    (   "_parser_ is reserved",
        {'_parser_': None, 'START': Token("x")}),
    (   'START symbol may not have dictionary elements',
        lambda: (lambda t: {'START': (t.__setitem__("a", 1), t)[1]})(Token("x"))),
    (   'Ref.* may not have dictionary elements',
        lambda: (lambda r: {'x': Token("x"), 'START': Choice((r.__setitem__("a", 1), r)[1])})(Ref("x"))),
    (   "left operand of \* must be one of",
        lambda: {"START": object() * Token('x')}),
]


#
# Test assert's and NotImpmenetedError and other such crap.
#
def test_notimplemented():
    def not_implemented(func):
        try:
            func()
            assert False
        except NotImplementedError:
            pass
    not_implemented(lambda: hash(lrparsing.Action()))
    not_implemented(lambda: lrparsing.Action() == lrparsing.Action())
    not_implemented(lambda: lrparsing.Action().precedence(None, None))
    def token_registry_del():
        del TokenRegistry()["x"]
    not_implemented(token_registry_del)
    def token_registry_set():
        TokenRegistry()["x"] = "x"
    not_implemented(token_registry_set)
    not_implemented(lambda: lrparsing.Symbol().compile_symbol(None))



if __name__ == "__main__":
  main()

# vim: set shiftwidth=4 expandtab softtabstop=8 :
