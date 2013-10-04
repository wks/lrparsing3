..
  Copyright (c) 2013 Russell Stuart.
  Licensed under GPLv2, or any later version.  See COPYING for details.

.. module:: lrparsing
    :synopsis: An LR(1) parser hiding behind a pythonic interface.

**************
|lrparsing|
**************

.. toctree::
   :maxdepth: 2

.. topic:: Abstract

   |Lrparsing| is an LR(1) parser hiding behind a pythonic interface.

:Author: Russell Stuart <russell-lrparsing@stuart.id.au>


Introduction
============

Lrparsing provides both an :term:`LR(1) parser <LR(1)>` and a
:term:`tokeniser`.  It differs from other Python LR(1) parsers in
using Python expressions as grammars, and offers simple to use
disambiguation tools.

The result is something that is powerful yet concise.  For simple
tasks this means it can be thought of as an extension to Python's
existing :mod:`re` module, used when regular expressions become
too cumbersome.   For complex tasks lrparsing offers a high speed
parser (very roughly 25us per token from string to :term:`parse tree`
on a desktop CPU), pre-compilation of the grammar and error recovery
hooks so parsing can continue after an error is found.

In addition to extensive documentation it comes with a parser for
Sqlite3 data manipulation statements and a Lua 5.2 to Python
compiler as examples.  The documentation can be read online at
http://www.stuart.id.au/russell/files/lrparsing/doc.


Quick Start
===========

For the impatient, here is a quick example which should suffice to get
you though simple use cases.  The :term:`grammar` is specified by a
class derived from :class:`Grammar`.  Members of that class define the
:term:`grammar`::

    import lrparsing
    from lrparsing import Keyword, List, Prio, Ref, THIS, Token, Tokens

    class ExprParser(lrparsing.Grammar):
        #
        # Put Tokens we don't want to re-type in a TokenRegistry.
        #
        class T(lrparsing.TokenRegistry):
            integer = Token(re="[0-9]+")
            integer["key"] = "I a mapping!"
            ident = Token(re="[A-Za-z_][A-Za-z_0-9]*")
        #
        # Grammar rules.
        #
        expr = Ref("expr")                # Forward reference
        call = T.ident + '(' + List(expr, ',') + ')'
        atom = T.ident | T.integer | Token('(') + expr + ')' | call
        expr = Prio(                      # If ambiguous choose atom 1st, ...
            atom,
            Tokens("+ - ~") >> THIS,      # >> means right associative
            THIS << Tokens("* / // %") << THIS,
            THIS << Tokens("+ -") << THIS,# THIS means "expr" here
            THIS << (Tokens("== !=") | Keyword("is")) << THIS)
        expr["a"] = "I am a mapping too!"
        START = expr                      # Where the grammar must start
        COMMENTS = (                      # Allow C and Python comments
            Token(re="#(?:[^\r\n]*(?:\r\n?|\n\r?))") |
            Token(re="/[*](?:[^*]|[*][^/])*[*]/"))

    parse_tree = ExprParser.parse("1 + /* a */ b + 3 * 4 is c(1, a)")
    print(ExprParser.repr_parse_tree(parse_tree))

Will print::

  (START (expr                  # Here START is ExprParser.START
    (expr                       # And expr is ExprParser.expr too.
      (expr
        (expr (atom '1')) '+'   # '+' is repr(input_matched_by_token)
        (expr (atom 'b'))) '+'  # 'b' is actually ExprParser.T.ident
      (expr
        (expr (atom '3')) '*'   # '3' is actually ExprParser.T.integer
        (expr (atom '4')))) 'is'
    (expr (atom (call 'c' '('
      (expr (atom '1')) ','     # ',' is an anonymous Token
      (expr (atom 'a')) ')')))))

As is suggested by the output, the :term:`parse tree` returned by
:func:`parse` is a tree composed of nested Python :func:`tuples <tuple>`.
The quoted strings above are actually :func:`tuples <tuple>` of the
form::

  (Token(), 'matched_input', offset_in_stream, line_number, column_number)

So the tuple representing ``'3'`` would be 
``(ExprParser.T.integer, '3', 16, 1, 17)``.


.. _module:

Module contents
===============

The module contains many classes and a few functions.  The classes
are described below, lumped into the following categories:

  * Define a :term:`grammar` in a subclass of :class:`Grammar`.

  * :class:`Symbol` classes describe rules in the grammar.

  * :class:`Token <TokenSymbol>` classes break up the input into
    :term:`input tuples <input tuple>`, which is what the
    :term:`parser` wants to be fed.

  * A :class:`TokenRegistry` manages :term:`tokens <token>`, the
    :term:`tokeniser`, and the
    :ref:`inbuilt tokeniser <inbuilt-tokeniser>`.

  * :exc:`Exceptions <LrParsingError>`.

In addition, there are a few utility routines.  They all duplicate
methods of the :class:`Grammar` class.  They are provided in case a
:class:`Grammar` subclass redefines them:

.. function:: compile_grammar(grammar)

    Compile ``grammar``, which must be a subclass of :class:`Grammar`.
    If the compile fails a :exc:`GrammarError` will be raised.  This
    it typically used when debugging new :class:`Grammars <Grammar>`.
    ``Grammar`` must be compiled before it can be used, but this is
    done implicitly for you by :func:`parse` or can be done
    explicitly by :func:`pre_compile_grammar()` if compiling is too
    slow.  :func:`repr_parse_table` displays a lot more information
    when the ``grammar`` is compiled by :func:`compile_grammar`.

.. function:: epoch_symbol(grammar)

    Returns the :term:`start symbol`, which is the :class:`Symbol` used
    to initialise the :class:`Grammar` subclass ``grammar``.  It is a
    :term:`non-terminal` with a single :term:`production`:
    ``<MyGrammar> = START __end_of_input__``.  Useful during
    :ref:`error-recovery`.

.. function:: parse(grammar, input, tree_factory=None, on_error=None, log=None)

    Parse the ``input`` using the subclass of :class:`Grammar` passed,
    returning a :term:`parse tree` if the ``grammar`` matches the
    input or raising a :exc:`ParseError` otherwise, calling
    :func:`compile_grammar` first if the ``grammar`` hasn't been
    compiled.  If the function ``on_error`` isn't :data:`None` it will
    be called when the parser input doesn't match the ``grammar``.
    It is documented in :ref:`error-recovery`.  If the function ``log``
    is passed it will be called repeatedly with :func:`strings <str>`
    showing parser's :term:`stack`.

    The ``input`` is passed to a :term:`tokeniser` which converts it
    to the stream of :term:`input tuples <input tuple>` expected by
    the :term:`parser`.  This :term:`tokeniser` is created by the
    :class:`TokenRegistry` declared in the :class:`Grammar` subclass.
    By default the :ref:`inbuilt tokeniser <inbuilt-tokeniser>` is used,
    and it accepts either a :func:`string <str>` or a generator that
    yields :func:`strings <str>` and :term:`input tuples <input tuple>`.
    :term:`Input tuples <input tuple>` are normal Python
    :func:`tuples <tuple>` that have a :class:`TokenSymbol` instance
    as their first element.  The :term:`parser` ignores the remaining
    elements in the :func:`tuple <tuple>`.

    The default format of the returned :term:`parse tree` is a tree of
    nested :func:`tuples <tuple>`.  These :func:`tuples <tuple>` will
    be one of the :term:`input tuples <input tuple>` given to the
    :term:`parser`, or will have one of the :class:`Rule` instances
    defined as a rule in the :class:`Grammar` followed by the
    :func:`tuples <tuple>` representing the :class:`Symbols <Symbol>`
    that made up the right hand side of the :term:`production` matched.
    (Use :func:`repr_productions` to see these productions.) The format
    of the returned :term:`parse tree` can be altered by passing a
    function as ``tree_factory``.  Whatever it returns is substituted
    into the :term:`parse tree` instead the :func:`tuple <tuple>`.
    It must accept one argument: the :func:`tuple <tuple>` that would
    have been inserted.

.. function:: pre_compile_grammar(grammar, pre_compiled=None)

    :func:`compile_grammar` can take a long while for reasonable sized
    grammars (seconds on a 32 bit out-of-order desktop CPU).  This
    function compiles the ``grammar`` (:class:`Grammar` subclass) and
    returns it as the :func:`repr` of a Python expression which is make
    up entirely of Python constants so you can insert the expression
    directly into your code.  If this return value or it's
    :func:`evaluation <eval>` is passed as ``pre_compiled`` to a future
    call and nothing has changed in the interim (eg, the ``grammar``)
    then the compile step is skipped and :data:`None` is returned,
    otherwise a new version of the compiled ``grammar`` is returned.
    If the grammar has already been pre compiled the call does nothing
    and :data:`None` is returned.  If :data:`None` is returned it is
    typically over 10 times faster than compiling the :class:`Grammar`
    from scratch.  If you insert the actual expression directly into
    your code (so it is compiled and becomes part of your .pyc file)
    it is over 100 times faster.  Pre compiling discards all
    information not directly used by the :term:`parser`, so it will
    save RAM and the parser runs a few percent faster.  However, the
    discarded information can be useful for debugging
    :class:`grammars <Grammar>`.

.. function:: repr_grammar(grammar)

    Return a printable :func:`string <str>` with the
    :class:`Rules <Rule>` as seen by the :class:`Grammar`
    subclass ``grammar``.  Useful for debugging
    :class:`grammars <Grammar>`.

.. function:: repr_parse_table(grammar, state=None)

    Return printable :func:`string <str>` describing the
    :term:`parse table` of the passed :class:`Grammar` subclass
    ``grammar``.  This only returns something useful after
    :func:`compile_grammar` has been called.  The return value
    may even be useful if :func:`compile_grammar` raised an exception.
    The result returned by a
    :func:`pre compiled <pre_compile_grammar>` ``grammar`` is barely
    useful for debugging the pre compiler.  If ``state`` (an
    :func:`integer <int>`) is passed only that state instead of the
    entire table is returned.  If ``state`` is negative that
    :term:`state` and all :term:`states <state>` that refer to it in
    their :term:`action` or :term:`goto tables <goto>` are returned.

.. function:: repr_parse_tree(parse_tree, indent="  ")

    Return a printable :func:`string <str>` representing the passed
    ``parse_tree`` (as returned by :func:`parse`) in a human readable
    format.  If ``indent`` is :data:`False` it will be on one line,
    otherwise it will be split across lines and indented using the
    :func:`string <str>` passed as ``indent`` to reveal the tree
    structure.

.. function:: repr_productions(grammar)

    Return a printable :func:`string <str>` describing all the
    :term:`productions <production>` in the passed :class:`Grammar`
    subclass ``grammar``.  This only works if :func:`compile_grammar`
    has been called.  It may work even if :func:`compile_grammar`
    has raised an exception.  Useful for debugging
    :class:`grammars <Grammar>`.

.. function:: unused_rules(grammar)

    Returns :class:`frozenset` of rules that aren't reachable from
    the :attr:`Grammar.START` rule.  If this isn't empty the
    ``grammar`` probably contains an error.

The Grammar Class
=================

A :term:`grammar's <grammar>` rules are contained in a class derived
from :mod:`lrparsing's <lrparsing>` :class:`Grammar` class. 
Members of the :class:`Grammar` subclass that are assigned instances
of the :class:`Symbol` class (described below) make up the rules of
the :term:`grammar`.  For the most part other members are simply
ignored by :mod:`lrparsing`, meaning you can add your own functions
and variables to a :class:`Grammar` subclass with impunity.

.. class:: Grammar()

    The constructor does nothing by default.
    :mod:`Lrparsing <lrparsing>` uses classes rather than instances
    to do its work, so it doesn't expect or care if a subclass is
    instantiated.
  
    .. classmethod:: compile_grammar(optimise=True, cache=None)

        Identical to the module's :func:`compile_grammar`.  If
        redefined the module's function continues to work.

    .. classmethod:: epoch_symbol()

        Identical to the module's :func:`epoch_symbol`.  If redefined the
        module's function continues to work.

    .. classmethod:: parse(input, tree_factory=None, log=None)

        Identical to the module's :func:`parse`.  If redefined the 
        module's function continues to work.

    .. classmethod:: pre_compile_grammar(, pre_compiled=None)

        Identical to the module's :func:`pre_compile_grammar`.  If
        redefined the module's function continues to work.

    .. classmethod:: repr_grammar()

        Identical to the module's :func:`repr_grammar`.  If redefined
        the module's function continues to work.

    .. classmethod:: repr_parse_tree(parse_tree, indent="  ")

        Identical to the module's :func:`repr_parse_tree`.  If redefined
        the module's function continues to work.

    .. classmethod:: repr_parse_table(state=None)

        Identical to the module's :func:`repr_parse_table`.  If
        redefined the module's function continues to work.

    .. classmethod:: repr_productions()

        Identical to the module's :func:`repr_productions`.  If
        redefined the module's function continues to work.

    .. classmethod:: unused_rules()

        Identical to the module's :func:`unused_rules`.  If redefined
        the module's function continues to work.

    .. attribute:: _parser_

        Reserved for internal use by :class:`Grammar`.  Do not use.

    .. attribute:: COMMENTS

        This member must be assigned a :func:`Keyword`, :class:`Token`,
        :func:`UnrecognisedToken` or :class:`UserToken`, or a
        :class:`Choice` of those symbols.
        :class:`TokenSymbols <TokenSymbol>` assigned to 
        :attr:`~Grammar.COMMENTS` are ignored by :func:`parse`.

    .. attribute:: START

        :attr:`~Grammar.START` must assigned a :class:`Rule` instance.
        The goal of the grammar is to recognise the :class:`Rule`
        defined by :attr:`~Grammar.START`.

    .. attribute:: WHITESPACE

        This is passed unaltered to the :term:`tokeniser`.  The
        :ref:`inbuilt tokeniser <inbuilt-tokeniser>` insists that if
        this is present, it is a string whose characters can optionally
        separate :class:`Tokens <Token>`.

Assigning a :class:`Symbol` to a member of a :class:`Grammar` subclass
creates a new :class:`Rule` instance containing the :class:`Symbol` on
the right hand side, and whose :attr:`Rule.name` is name of the member
it was assigned to.  This makes it different from normal Python
assignment where the left hand side and the right hand side are the
same object after the assignment executes.  In practice there are two
places this matters.  Firstly, except for assigning to
:attr:`Grammar.START`, you may not assign one :class:`Rule` directly
to another as it makes it impossible to determine the grammar hierarchy
[#grammar_rule_assignment]_. Secondly, you cannot get a reference to
a :class:`TokenSymbol` by assigning it to a rule.  Use a
:class:`TokenRegistry` to do that [#token_registry_example]_.
:class:`Rules <Rule>` whose :attr:`names <Rule.name>` start with an
underscore (``_``) are treated specially.  They are suppressed in the
output :term:`parse tree` [#grammar_underscore_example]_.

Symbol Classes
==============

Instances of :class:`Symbol` subclasses are used to build rules in
a :class:`Grammar`.  :class:`Symbol` instances can be created in the
normal way - by calling their class name (aka the constructor) with
the appropriate arguments.  However :mod:`lrparsing` provides some
syntactic sugar by hijacking the Python expression builder.  The
Python operators that are translated to calling a :class:`Symbol`
constructor are noted below.  Another sweetener is all
:class:`Symbol` class constructors will, when they are expecting a
:class:`Symbol` as an argument, cast some standard Python types to
an appropriate :class:`Symbol` class instance.  For example, a
:func:`string <str>` will be cast to a literal :class:`Token`.
[#symbol_example]_

The :class:`Symbol` subclass constructors that can be used to define
:class:`Grammars <Grammar>` are:

    .. class:: Choice(s0, s1, ...)

        Alternate syntax: ``s0 | s1 | ...``

        The :term:`grammar` must choose between one of the
        :class:`symbols <Symbol>` s0, s1, ...

    .. class:: Left(s)

        Alternate syntax: ``s = s0 << s1 << s2 << ...``

        If the :class:`symbol <Symbol>` ``s`` is a
        ``Sequence(s0, s1, s2, ...)``, and there is a choice between the
        :term:`parse trees <parse tree>` ``(((s0 s1) s2) ...)`` and
        ``(... (s0 (s1 s2)))`` use the ``(((s0 s1) s2) ...)``.  In other
        words ``s`` is left associative.

    .. class:: List(s, d, min=0, max=None, opt=False)

        The :term:`grammar` must see a list of the :class:`symbol <Symbol>`
        ``s`` separated by the :class:`symbol <Symbol>` ``d``, ie,
        ``s d s d s ...``.  If ``opt`` is :data:`True` the list may
        optionally end with ``d``, otherwise it must end with ``s``.
        If ``min`` is absent or 0 the list may be empty, otherwise
        ``s`` must appear a minimum of ``min`` times.  If ``max`` is
        :data:`None` any number of ``s`` are allowed, otherwise
        ``max`` is the maximum number of occurrences of ``s``.

    .. class:: Nonassoc(s)

        If the :class:`symbol <Symbol>` ``s`` is a
        ``Sequence(s0, s1, s2, ...)``, and there is a choice between the
        :term:`parse trees <parse tree>` ``(((s0 s1) s2) ...)`` and
        ``(... (s0 (s1 s2)))`` raise a :exc:`ParseError`.  In other words
        ``s`` isn't associative.

    .. class:: Prio(s0, s1, ..)

        Alternate syntax: ``(s0, s1, ...)``

        The :term:`grammar` must choose between one of the symbols
        ``s0, s1, ...`` If several choices are possible, choose ``s0`` over
        ``s1`` over ``...`` A :func:`tuple <tuple>` assigned directly to
        a class member is ignored (as any non :class:`Symbol` is), but
        will be converted to a :class:`Prio` if passed to a :class:`Symbol`
        constructor.

    .. class:: Ref(name)

        A forward reference to the :class:`Rule` assigned to the class
        member ``name`` (a :func:`string <str>`).  It will be replaced by
        that member definition, which must occur later.

    .. class:: Repeat(s, min=0, max=None)

        Alternative syntaxes:
          * ``s * N``
          * ``s * (min,)``
          * ``s * (min,max)``
          * ``Opt(s)``, ``s * Opt``, ``s * (0,1)``
          * ``Some(s)``, ``s * Some``, ``s * (1,)``
          * ``Many(s)``, ``s * Many``, ``s * ()``

        The :term:`grammar` must see ``s`` repeated.  If ``min`` is 0 or
        absent no repeats are allowed meaning there may be no ``s`` at all,
        otherwise it must appear a minimum of ``min`` times.  If ``max`` is
        :data:`None` any number of ``s`` are allowed, otherwise ``max``
        is the maximum number of repeats.  These shortcuts are also
        provided:

            * ``Opt(x)`` or ``x * Opt`` is equivalent to ``Repeat(x, min=0, max=1)``.
            * ``Some(x)`` or ``x * Some`` is equivalent to ``Repeat(x, min=1)``.
            * ``Many(x)`` or ``x * Many`` is equivalent to ``Repeat(x)``.
            * ``x * N`` is equivalent to ``Repeat(x, min=N, max=N)``.
            * ``x * tuple(...)`` is equivalent to ``Repeat(x, *tuple(...))``.

    .. class:: Right(s)

        Alternative syntax: ``s = s0 >> s1 >> s2 >> ...``

        If the :class:`symbol <Symbol>` ``s`` is a
        ``Sequence(s0, s1, s2, ...)``, and there
        is a choice between the :term:`parse trees <parse tree>`
        ``(((s0 s1) s2) ...)`` and ``(... (s0 (s1 s2)))`` use the
        ``(... (s0 (s1 s2)))``.  In other words ``s`` is right associative.

    .. class:: Sequence(s0, s1, ...)

        Alternative syntax: ``s + s1 + ...``

        The :term:`grammar` must see :class:`symbol <Symbol>` ``s0``,
        followed by ``s1``, followed by ...

    .. class:: THIS

        A reference to the :class:`Rule` being assigned to.  So identical
        to ``Ref('this')`` in ``this = Ref('this') + '+' + Ref('this')``.

    .. class:: Tokens(literals, [keywords])

        The :term:`grammar` must choose one of the supplied literals and
        keywords recognised by the
        :ref:`inbuilt tokeniser <inbuilt-tokeniser>`.  Both ``literals``
        and ``keywords`` are strings which are ``string.split()`` to yield
        multiple literals and keywords, each of which is recognised using
        the inbuilt :class:`Token` class.

Python does not allow ``rule =`` for an empty rule.  Use
``rule = THIS * 0`` instead.

Although the classes above are used to build :class:`Grammar` rules
by appearing on the right hand side of a Python assignment statement
their instances are not directly visible.  Instead an instance of
the :class:`Rule` class is created to hold them, and this is what is
assigned to your :class:`Grammar` attribute.  Since they are a
subclass of :class:`Symbol` they can be referenced by other grammar
rules:

  .. class:: Rule()

      .. attribute:: name

          The name of the :class:`Grammar` attribute the :class:`Rule`
          was assigned to.  A :func:`string <str>`.

      .. method:: __str__

          Returns :attr:`name`.

All :term:`symbols <symbol>` the :term:`parser` can recognise
are derived from this abstract base class, and thus share these
attributes:

  .. class:: Symbol()

      If you override :class:`Symbol` the constructor must be called
      as it does initialisation.

      .. attribute:: dict

          A :class:`dict` you can use for your own purposes for all
          rules bar :class:`Grammar.START`.

      .. method:: __contains__(key):

          Equivalent to ``key in symbol.dict``.

      .. method:: __delitem__(key):

          Equivalent to ``del symbol.dict[key]``.

      .. method:: __getitem__(key):

          Equivalent to ``symbol.dict[key]``.

      .. method:: __iter__():

          Equivalent to ``iter(symbol.dict)``.

      .. method:: __len__():

          Equivalent to ``len(symbol.dict)``.

      .. method:: __nonzero__():

          Always returns :data:`True`, ensuring a :class:`Symbol`
          instance is always truthy.

      .. method:: __setitem__(key, value)

          Equivalent to ``symbol.dict[key] = value``.

      .. method:: __repr__()

          Returns the production for this :class:`Symbol`.

      .. method:: __str__()

          Generates a unique name of the :class:`Symbol`, based
          on the :class:`Rule` it belongs to.
          :class:`Tokens <TokenSymbol>` override this to return
          their :attr:`~TokenSymbol.name` attribute.

Tokens
======

:term:`Tokens <Token>` are a special kind of :class:`Symbol`
[#tokens]_.  As a convenience the same token may have several
instances in a :class:`Grammar`.  Instances of the same
:class:`token <TokenSymbol>` are merged during the compile, resulting
in all bar one being discarded.  Two :class:`tokens <TokenSymbol>`
are considered to be the same if they have the same
:attr:`~TokenSymbol.name` attribute.  The inbuilt tokens generate a
suitable :attr:`~TokenSymbol.name` for you, but you can assign a
different name to any :class:`token <TokenSymbol>` using a
:class:`TokenRegistry`.  :attr:`Names <TokenSymbol.name>` containing
``__`` are used internally, and so are reserved.

The following methods and classes are used directly in
:class:`grammars <Grammar>`:

  .. function:: Keyword(word, case=True)

      Create an instance of the :class:`Token` class that recognises a
      keyword.  If case is ``False`` the keyword isn't case sensitive,
      otherwise it is.  The default :attr:`~TokenSymbol.name` is
      :func:`repr(word) <repr>`.  A keyword is a literal :class:`Token`
      that is recognised by another re :class:`Token`.  For example,
      an identifier might be recognised by ``Token(re='[A-Za-z_]+')``.
      The keyword ``'then'`` would normally be recognised by the
      identifier :class:`Token`, but if ``Keyword('then')`` is
      present it will match ``'then'``, while all other non-keywords
      will still be matched by the identifier :class:`Token`.

  .. class:: Token(literal=None, re=None, case=True)

      Alternative syntax: ``'literal'``

      Use the :ref:`inbuilt tokeniser <inbuilt-tokeniser>` to generate
      a :class:`Token` symbol when it sees the text specified by
      ``literal`` or ``re``.  The arguments becomes attributes with
      the name.  You can only supply ``literal`` or ``re``, not both.
      If ``literal`` is supplied the :term:`tokeniser` will match
      exactly that text unless ``case`` is :data:`False` in which case
      it ignores case when matching.  If ``re`` is supplied it must be
      a regular expression the :term:`tokeniser` will match.  The ``re``
      is compiled with :data:`re.MULTILINE` and :data:`re.DOTALL`;
      adding additional flags using :mod:`re's <re>` ``(?FLAG)`` syntax
      affects the recognition of *all* :class:`Tokens <Token>`.  The
      default :attr:`~TokenSymbol.name` for literal tokens is
      :func:`repr(literal) <repr>`.  The default
      :attr:`~TokenSymbol.name` for re tokens is the ``re`` surrounded
      by ``/``'s.  Strings assigned directly to class members are
      ignored as are all non :class:`Symbols <Symbol>`, but strings
      passed to :class:`Symbol` constructors are turned into a literal
      :class:`Token`.

  .. class:: UserToken()

      A user defined token.  The :class:`UserToken` must be assigned a
      :attr:`~TokenSymbol.name` by a :class:`TokenRegistry`.  This class
      can be used as a base for user defined token classes.

  .. function:: UnrecognisedToken()

      Create an instance of the :class:`Token` class that is given to the
      :term:`parser` when the :ref:`inbuilt tokeniser <inbuilt-tokeniser>`
      strikes a string it doesn't recognise as a token or whitespace.  If
      this token doesn't appear in the :class:`Grammar` the
      :ref:`inbuilt tokeniser <inbuilt-tokeniser>` raises a
      :exc:`TokenError` instead.

If you delve deeply into |lrparsing| you will come across these token
classes.  They aren't directly used by :class:`grammars <Grammar>`:

  .. class:: MetaToken(name)

      Used to construct special tokens used internally by the
      :term:`grammar compiler <parser generator>` and
      :term:`parser`.  You will never see these in the output 
      :term:`parse tree`, but the :func:`tree_factory <parse>`
      and :ref:`error recovery <error-recovery>` may see them.
      There are two: ``__empty__`` represents a 0 width character
      string, and ``__end_of_input__`` represents the end of the
      input stream.

  .. class:: TokenSymbol(name)

      The abstract base class all :term:`tokens <token>` are based on,
      thus ``isinstance(symbol, TokenSymbol)`` will return :data:`True`
      if a :class:`Symbol` is a :term:`token` of some kind.  The passed
      ``name`` is the generated name which can be overridden by the
      :class:`TokenRegistry`.  :class:`TokenSymbols <TokenSymbol>`
      and thus all subclasses have these attributes and methods:

      .. attribute:: dict

          This is inherited from :class:`Symbol`, as is the shorthand
          access provided by :class:`Symbol's <Symbol>` mapping interface.  
          :attr:`TokenSymbol.dict` is best set from within a
          :class:`TokenRegistry`.  Changes made to this attribute
          in the body of a :class:`Grammar` subclass change the
          :class:`Symbol.dict` of the :class:`Rule` the :class:`TokenSymbol`
          is assigned to, not the :class:`TokenSymbols <TokenSymbol>`
          :attr:`~TokenSymbol.dict`.

      .. attribute:: name

          A :func:`string <str>`.  The name of the token.  If two tokens
          with the same :attr:`~TokenSymbol.name` are used in a
          :class:`Grammar` they will be merged using
          :meth:`~TokenSymbol.merge` and only one will be kept.

      .. attribute:: named

          A :func:`bool <bool>`.  :data:`True` if
          :meth:`~TokenSymbol.set_name` has been called.

      .. method:: merge(other)

          Called by the :class:`TokenRegistry` when two tokens have the same
          :attr:`~TokenSymbol.name`.  ``Other`` will be discarded after merge
          returns.

      .. method:: position(input_tuple)

          Given an ``input_tuple`` supplied to the parser, return a
          :func:`string <str>` describing the position in the input stream.
          The default implementation returns :data:`None`, which means
          if the ``input_tuple`` triggers an error in the
          :func:`parser <parse>` it won't insert the position in the input
          stream of the error.
          
      .. method:: set_name(name)

          Called by the :class:`TokenRegistry` to override the
          generated :attr:`~TokenSymbol.name`.  Sets
          :attr:`~TokenSymbol.named` to :data:`True`.

      .. method:: __str__()

          Returns :attr:`str(name) <TokenSymbol.name>`.

The TokenRegistry Class
=======================

The :class:`TokenRegistry` is the home for tokens.  If you don't
define subclass within the :class:`Grammar` it will use an instance
of :class:`TokenRegistry` directly [#token_registry_example]_.
Assigning a :class:`token <TokenSymbol>` to a member of a
:class:`TokenRegistry` class gives you a handle to the
:class:`token <TokenSymbol>` instances recognised by the
:class:`Grammar`, and it is the only way to get such a handle
[#token_registry_corollary]_.  Assigning the
:class:`token <TokenSymbol>` also sets the :attr:`TokenSymbol.name`
of the :class:`token <TokenSymbol>` to the quanified attribute name,
which is a :func:`string <str>` with the format
``'NameOfTokenRegistrySubclass.member_name'``.

    .. class:: TokenRegistry()

        A single instance of the a :class:`TokenRegistry` or the subclass
        defined in the :class:`Grammar` is created when the class when the
        :class:`Grammar` is declared.  The constructor looks up all tokens
        declared in its body and registers their :attr:`TokenSymbol.name`
        using :meth:`~TokenRegistry._resolve_token_`.

        .. method:: _resolve_token_(token_symbol)

            Called when the :class:`Grammar` is compiled into
            :term:`LR(1) productions <production>` as :class:`tokens <Token>`
            are found.  It returns the :class:`TokenSymbol` that will be
            presented to the parser for :attr:`TokenSymbol.name`.  That
            instance will always be the first :class:`TokenSymbol` passed to
            it with the :attr:`TokenSymbol.name`.  If the passed token is a
            duplicate, it will pass it to :meth:`TokenSymbol.merge` before
            discarding it.

        .. method:: compile_tokens(whitespace)

            Called once, during :class:`Grammar` compilation and before the
            first call to :meth:`~TokenRegistry.tokeniser`, so the
            :term:`tokeniser` can initialise itself.  The ``whitespace``
            parameter is the :attr:`Grammar.WHITESPACE` attribute if one was
            declared, :data:`None` otherwise.  This method must raise an
            exception if ``whitespace`` isn't valid.  What "whitespace"
            means is up to the :term:`tokeniser`.  The default definition
            creates an instance of the
            :ref:`inbuilt tokeniser <inbuilt-tokeniser>`, and calls its
            ``compile_tokens`` method with the same arguments.

        .. method:: tokeniser(input, whitespace)

            This is a generator, called by :func:`parse` to create
            :term:`input tuples <input tuple>` to form the input to feed to
            the :term:`LR(1) parser <parser>` from the ``input`` parameter
            passed to :func:`parse`.  The ``whitespace`` parameter is the
            same one that was passed to :meth:`~TokenRegistry.compile_tokens`.
            The :term:`input tuples <input tuple>` accepted by the parser are 
            :func:`tuples <tuple>` whose first element is a
            :class:`TokenSymbol` instance.  The remainder of the
            :func:`tuple <tuple>` is ignored by the :term:`parser`, but
            the :func:`tuple <tuple>` is placed unmodified into the output
            :term:`parse tree`.  The default definition calls the
            :meth:`~TokenRegistry.tokeniser` method of the
            :ref:`inbuilt tokeniser <inbuilt-tokeniser>`, passing it the
            same arguments [#tokeniser-override]_.


.. _inbuilt-tokeniser:

Inbuilt Tokeniser
-----------------

The inbuilt tokeniser generates :class:`Token` instances.  Thus
if you use :func:`Keyword`, :class:`Token`, :class:`Tokens` or
:func:`UnrecognisedToken` in your grammar, you must use the inbuilt
tokeniser.  It is used by default unless you add a
:class:`TokenRegistry` to your grammar and override its
:meth:`TokenRegistry.compile_tokens` and
:meth:`TokenRegistry.tokeniser` methods.

The input accepted by the inbuilt :term:`tokeniser` is either a
:func:`string <str>` or a generator yielding a mixture of strings and
:term:`input tuples <input tuple>` [#tokeniser_chain]_.  If the input
is broken up into multiple strings you should ensure the breaks occur
at :term:`token` boundaries.  :attr:`Grammar.WHITESPACE` is a
:func:`string <str>` whose characters can appear between
:class:`tokens <Token>` but are otherwise ignored.  If not defined it
defaults to ``" \f\n\r\t\v"`` is used.


:func:`Strings <str>` are turned into :term:`input tuples <input tuple>`
for the parser with the following format::

  (Token(), matched_data, stream_offset, line_number, column_number)

where:

  ``Token()``
      is an instance of the :class:`Token` class used in the 
      :class:`Grammar`.

  ``matched_data``
      is the matched :func:`string <str>` from the input. A
      :func:`string <str>`.

  ``stream_offset``
      is the offset from the start of the input where the matched data
      was found. An :func:`integer <int>`.

  ``line_number``
      is the line number the string was found on, counting from 1.
      An :func:`integer <int>`.

  ``column_number``
      is the column number within the line where the string was found,
      starting at 1.  An :func:`integer <int>`.

If a generator is passed and it yields objects other than strings they
are passed onto the :term:`parser` unchanged in the order they were
received.


Exceptions
==========

.. exception:: LrParsingError

    Base: :exc:`StandardError`.

    An abstract base class used by all exceptions defined here.

.. exception:: GrammarError

    Base: :exc:`LrParsingError`.

    Raised when there is an error in the grammar.  This can happen both
    when the grammar is first parsed, and when :func:`compile_grammar`
    is called.

.. exception:: ParsingError

    Base: :exc:`LrParsingError`.

    An abstract base class for all errors that can be raised by this
    module when :func:`parse` is called.

.. exception:: TokenError(message, data, offset, line, column)

    Base: :exc:`ParsingError`.

    Raised by the :ref:`inbuilt tokeniser <inbuilt-tokeniser>` when it
    strikes something in the input that isn't entirely whitespace,
    isn't a recognisable :class:`Token`, and the grammar doesn't use
    :class:`UnrecognisedToken`.  The arguments following ``message``
    become attributes of the instance.

    .. attribute:: data

        The :func:`string <str>` that wasn't recognised.

    .. attribute:: offset

        :attr:`Data's <data>` position in the input stream
        as a character offset from the start.  An
        :func:`integer <int>`.

    .. attribute:: line

        :attr:`Data's <data>` position in the input stream
        as a line number starting at 1 for the first line in the
        input. An :func:`integer <int>`.

    .. attribute:: column

        :attr:`Data's <data>` position in the input stream
        as the column number in the current line, starting at 1 for the
        first column. An :func:`integer <int>`.

.. exception:: ParseError(input_tuple, lr1_state)

    Base: :exc:`ParsingError`.

    Raised by :func:`parse` when the input the :term:`parser` is given
    doesn't match the :class:`Grammar`.  The arguments become instance
    attributes with the same name.

    .. attribute:: input_tuple

        The :term:`input tuple` given to the parser that triggered the
        error.  If this :term:`input tuple` was created by the
        :ref:`inbuilt tokeniser <inbuilt-tokeniser>` it will contain
        position information, ie, line number, column number and so on.

    .. attribute:: lr1_state

        The current parser state, an :class:`Lr1State` instance
        [#error-recovery-lie]_, from the parse table (ie, as returned
        by :func:`repr_parse_table`).  If the parse table hasn't been
        pre compiled :func:`repr` reveals a lot of information about
        what the parser was expecting at the time.

.. _error-recovery:

Error Recovery
==============

The default action of :func:`parse` is to raise a :exc:`ParseError`
exception which creates a suitable message, which of course stops
the :term:`parser` at that point.  If you want the :term:`parser`
to continue, for example so it can discover more errors in the
same run, you need to do error recovery.  Error recovery is always
accomplished by manipulating the :term:`parser's <parser>` input
so the parser thinks it has seen valid input, and then let it
continue.  It is implemented by the ``on_error`` parameter to
:func:`parse`:

.. function:: on_error(iterator, input_tuple, stack)

    Handle a parser error.

    :param input_tuple: The :term:`input tuple` that triggered the
           error.
    :param iterator: The iterator the parser is reading
           :term:`input tuples <input tuple>` from.
    :param stack: The parser's :term:`stack`.  A :func:`list <list>`.
    :rtype: :data:`None` or an iterable yielding
           :term:`input tuples <input tuple>` that will be inserted
           into the input stream.

The difficulty with any error recovery is determining how to turn the
mess you are given into something the :term:`parser` will accept.
Invariably this means you try to figure out what the user was trying
to do.  For example, let's say you are parsing Python and strike an
problem in an expression.  If the expression was part of a *for*
statement your best strategy is probably to replace the entire
*for* statement with *for x in ():*.  But if that *for* was inside
a list comprehension inserting a *for* statement at that point would
likely generate a cascade of errors later on.  So understanding where
you are is critical.  Sadly this is one thing :term:`LR(1)` makes
hard.  That information is buried in the ``stack`` parameter.  The
``stack`` is a partially constructed branch, running from the root to
a leaf, of a :term:`parse tree`.  It is a :func:`list <list>` of
:func:`tuples <tuple>`, like this::

    [
      (lr1_state, (symbol, ...)),
      (lr1_state, (symbol, ...)),
      ...
    ]

Consider the following two :term:`grammars <grammar>` which both
recognise the same thing, a small subset of the C language::

    >>> def on_error(iterator, token, stack):
        for item in stack:
            print "([%s], (%s))" % (
                ','.join(sorted(str(rule) for rule in item[0].rules)),
                ','.join(
                    lrparsing.repr_parse_tree(sym, indent="")
                    for sym in item[1]))

    >>> class Lang1(Grammar):
        e = Prio(
            Token(re='[0-9]+') | Token(re='[a-z]+') + Opt(Token('(') + THIS + ')'),
            THIS << '*' << THIS,
            THIS << '+' << THIS)
        START = Repeat((
            Opt(e) |
            Keyword("if") + e + Keyword('then') + THIS +
                Opt(Keyword('else') + Ref("START")) + Keyword("endif") |
            Keyword("while") + e + Keyword('do') + THIS + Keyword("done") |
            Token(re='[a-z]+') + '=' + e) + ';')

    >>> Lang1.parse(
        "e=1; while funca(e) do if a then e=a; fi; done", on_error=on_error)
    ([<Lang1>], (__empty__))
    ([START], ('e','=',(e '1'),';'))
    ([START], ('while'))
    ([START,e], ((e 'funca' '(' (e 'e') ')'))),
    ([START], ('do'))
    ([START], ('if'))
    ([START,e], ((e 'a')))
    ([START], ('then'))
    ([START], ((START 'e' '=' (e 'a') ';' (e 'fi') ';')))
    lrparsing.ParseError: line 1 column 43: Got 'done' when expecting 'else' or 'endif' while trying to match START in state 44

    >>> class Lang2(Grammar):
        class T(TokenRegistry):
            number = Token(re='[0-9]+')
            ident = Token(re='[a-z]+')
            kwd_endif = Keyword("endif")
        exp = Ref("exp")
        block = Ref("block")
        call = T.ident + '(' + exp + ')'
        mul_op = exp << '*' << exp
        add_op = exp << '+' << exp
        exp = Prio(T.number | T.ident | call, mul_op, add_op)
        if_part = Keyword("if") + exp
        then_part =  Keyword("then") + block
        else_part = Keyword("else") + block
        if_st = (if_part + then_part + Opt(else_part) + endif)
        while_part = Keyword("while") + exp
        do_part = Keyword("do") + block
        while_st = while_part + do_part + Keyword("done")
        exp_st = exp * 1
        ass_exp = exp * 1
        ass_st = T.ident + '=' + ass_exp
        empty_st = THIS * 0
        st = ass_st | empty_st | exp_st | if_st | while_st
        block = Repeat(st + ';')
        START = block

    >>> Lang2.parse(
        "e=1; while funca(e) do if a then e=a; fi; done", on_error=on_error)
    ([<Lang2>], (__empty__))
    ([block], ((st (ass_st 'e' '=' (ass_exp (exp '1'))),';'))
    ([while_st], ((while_part 'while' (exp (call 'funca' '(' (exp 'e') ')'))))
    ([do_part], ('do'))
    ([if_st], ((if_part 'if' (exp 'a'))))
    ([then_part], ('then'))
    ([then_part], ((block (st (ass_st 'e' '=' (ass_exp (exp 'a'))) ';' (st (exp_st (exp 'fi'))) ';'))),
    lrparsing.ParseError: line 1 column 43: Got 'done' when expecting 'else' or 'endif' while trying to match then_part in state 50

Here the :func:`on_error` function prints the ``stack`` passed, but
doesn't attempt recovery.  Looking at the right hand side of the
``stack``, the ``(symbol, symbol, ...)`` bit, it shouldn't be too hard
to see how stitching it together will yield a branch of the
:term:`parse tree`.  This sequence of :class:`symbols <Symbol>` must
eventually match one of the :class:`Rules <Rule>` listed to their left.
The :term:`LR(1)` parser may list several as it will only make its
final decision once it has seen the entire :term:`production`.  For
``Lang2`` it is not difficult to see the :term:`parser` was in the
middle of the *then_part* of an *if_st*.  ``Lang1`` is an example of
how to make recognising this difficult.

``lr1_state`` has more information in it than the above example
shows. It is an :class:`Lr1State` [#error-recovery-lie]_:

    .. class:: Lr1State(id, actions, gotos, rules)

        This class is immutable, the parameters becoming attributes of
        the same name.  It has no useful methods beyond
        :meth:`object.__str__` and :meth:`object.__repr__`.
            
        .. attribute:: id

            An :func:`integer <int>` identifing the :term:`state`.
            In an :class:`Lr1State` this integer is its index into the
            :term:`parse table`.
                
        .. attribute:: actions

            A :class:`dict`, indexed by all valid
            :class:`TokenSymbols <TokenSymbol>` that could appear at
            this point.  The error occurred because the current
            :term:`token` isn't a key to this :class:`dict`.

        .. attribute:: gotos

            The :term:`goto table <goto>` of this parse :term:`state`.
            A :class:`dict`.  Not useful for error recovery.

        .. attribute:: rules

            This is a :func:`list <list>` of :class:`Rules <Rule>`
            from your grammar the parser was in the process of
            recognising.  In other words if the parser was processing
            the grammar ``class G(Grammar): START = Token("x")``,
            then this must be :data:`True`:
            ``rules[0] in (G.START,G.epoch_symbol())``.

By just looking at the top item of the passed stack for ``Lang2`` 
we see the :term:`parser` was in the middle of a *then_block*.  Its
task is to re-arrange the input so the :term:`parser` can continue.
The simple way is to insert one of the
:class:`TokenSymbols <TokenSymbol>` out of :class:`Lr1State.actions`.
In this case ``'endif'`` would be the right choice.  Another choice
is to erase some input, possibly replacing it with something else.
This would be appropriate if, for example the string being parsed was
``((2 func 2;``.  Replacing ``func 2`` with ``))`` would get the
:term:`parser` onto the next statement so the user only gets one
error for the current line.

If :func:`on_error` returns :data:`None` the parser will raise a
:exc:`ParseError` as if :func:`on_error` wasn't passed to it.
Otherwise it will read :term:`input tuples <input tuple>` from the
returned iterable, and when that is exhausted return to reading
the original input.  So, returning a :func:`list <list>` of
:term:`input tuples <input tuple>` will insert them, reading
:term:`input tuples <input tuple>` off the passed ``iterator``
will delete them, and reading them and returning what you read
allows you to peek ahead.  As the :term:`parser` has already
processed the :term:`input tuple` it passed to :func:`on_error`,
if you want the :term:`parser` to reprocess it, it must be in the returned
iterable.  Going back to the example, this line would insert an
``'endif'``, thus allowing the :term:`parser` to process the
remainder of the input::

    endif = (Lang2.T.kwd_endif, Lang2.T.kwd_endif.literal) + token[2:]
    return [endif, token]

There is one other option for the adventurous.  The ``stack``
passed to :func:`on_error` is the actual :term:`parser stack <stack>`,
so you can modify it.  The :term:`states <state>` on the stack
depend on the whims of the :term:`parser generator` so adding or
modifying them is impossibly fragile.  Deleting them, using say
``del stack[-2:]`` is not.  In the ``((2 func 2`` example deleting
the entire expression from the :term:`stack` and presenting the
:term:`parser` with a replacement expression on its input (eg,
just ``Lang2.T.number``) is often simpler.


Ambiguities
===========

Lurking under |lrparsing|'s hood is an :term:`LR(1)` parser.
:term:`LR(1) parser generators <parser generator>` are cantankerous
beasts.  |Lrparsing| puts lipstick on the pig, but the
pig can still bite.  The principal way it bites is to say your
grammar is :term:`ambiguous`.  This section is here to help you
work past this error, but be aware writing large complex grammars
requires a fair amount of expertise which can't be imparted by this
small how-to.

To understand what ambiguous means, understand that a :term:`parser` is
meant to take some input, validate it conforms to the :term:`grammar`,
and then produce a :term:`parse tree`.  When the parser generator says
the :term:`grammar` is :term:`ambiguous`, it means that for some valid
input several different :term:`parse trees <parse tree>` can be
produced.  An :term:`LR(1)` :term:`grammar` requires there be only one
:term:`parse tree` for any given input.

A blatantly ambiguous grammar will produce a
:term:`Reduce/Reduce conflict <Reduce/Reduce>`.  Consider::

   class A(Grammar):
      a = Token('x')
      b = Token('x')
      START = a | b

``A.compile_grammar()`` raises this exception::

    lrparsing.GrammarError: Conflict: Reduce/Reduce
    While trying to recognise:
      b = 'x' ^
      a = 'x' ^
    on seeing a __end_of_input__ I could not decide between:
      replace the sequence ['x'] with b
    and
      replace the sequence ['x'] with a
    Please remove this ambiguity from your grammar

Looking at the error message, it displays every possible
:term:`production` that could apply at this point, with an :term:`^`
showing where the parser is up to in each :term:`production`.
There are only two types of :term:`actions <action>` the parser can
take.  It can accept the next token in the input stream.  This is
called a :term:`Shift`.  It isn't possible here as the next symbol is
``__end_of_input__``, in other words it is at the end of the string.
The other action it could take is replace some symbols with the
:term:`production` they match.  This is called a :term:`Reduce`.
It can do a reduce as there are two productions that match.
Sadly two is one too many, and this :term:`conflict` is called
:term:`Reduce/Reduce` because both options are
:term:`reduces <reduce>`.  They correspond to these
:term:`parse trees <parse tree>`::

    (START (a 'x'))  OR  (START (b 'x'))

Since there are two possible :term:`parse trees <parse tree>` the
grammar is :term:`ambiguous`.  If you look at the error message just
the right way, it could almost be said to say that.  The solution in
this case is to remove one of the redundant :class:`Rules <Rule>`::

   class A(Grammar):
      a = Token('x')
      START = a

Looping grammars are also ambiguous.  Consider this
:term:`grammar`::

   class B(Grammar):
       b = Ref("e")
       e = b | Token("x")
       START = e

Again, the :class:`Grammar` will recognise a single ``'x'``.
``B.compile_grammar()`` raises this exception::

    lrparsing.GrammarError: Conflict: Reduce/Reduce
    While trying to recognise:
      START = e ^
      b = e ^
    on seeing a __end_of_input__ I could not decide between:
      replace the sequence [e] with b
    and
      replace the sequence [e] with START
    Please remove this ambiguity from your grammar

The error message is saying when the :term:`parser` has seen a single
``'e'``, it has two choices, which are listed.  These two choices
correspond to an infinite number of :term:`parse trees <parse tree>`::

   (START (e 'x')) OR (START (e (b (e 'x')))) OR (START (e (b (e (b (e 'x')))))) OR ...

Again since there is more than one possible parse tree, the grammar is
:term:`ambiguous`.

Unfortunately those two simple examples don't capture the complexity of
common :term:`ambiguities <ambiguous>`. Consider this example which is
common in real world :term:`grammars <grammar>`::

   class E(Grammar):
       e = THIS + Tokens("+ /") + THIS | Token(re="[0-9]+")
       START = e

This :class:`Grammar` recognises strings of integers separated by ``'+'``
and ``'/'``.  If the grammar writer wanted to produce a grammar that only
recognised valid integer expressions using the operators ``'+'`` and
``'/'`` he was successful.  Nonetheless, when he calls
``E.compile_grammar()`` this exception is raised::

    lrparsing.GrammarError: Conflict: Shift/Reduce and no associativity
    While trying to recognise:
      e = e '+' e ^
      e = e ^ '/' e
    on seeing a '/' I could not decide between:
      replace the sequence [e '+' e] with e
    and
      accepting the '/' in the hope it will match:
        e = e '/' ^ e
    Please remove this ambiguity from your grammar
    
Again the error message lists the :term:`productions <production>`
under consideration and how far the parser has got in each.  The
smallest possible string the parser could be looking at and end up
with those possibles is: ``e + e ^ / e``.  The parser wants to know
whether should it replace (:term:`reduce`) ``e + e`` with e, which
corresponds to  performing the addition now, or should it continue
reading, ie, delay the addition until it processes the ``'/'``.
This is called a :term:`Shift/Reduce conflict <Shift/Reduce>`
because the two :term:`actions <action>` possible are :term:`Shift`
and :term:`Reduce`.  The two :term:`parse trees <parse tree>`
under consideration are thus::

     Choose Shift:  e + (e / e)
     Choose Reduce: (e + e) / e

The normal arithmetic convention is to do the division first, so we want
to choose ``Shift``.  There are several ways to make the grammar do that.
The easiest way in |lrparsing| is to prioritise the two choices, saying 
``e '/' e`` should always be done before ``e '+' e``::

   class E(Grammar):
       e = Prio(THIS + '/' + THIS, THIS + '+' + THIS) | Token(re="[0-9]+")
       START = e

But doing that only replaces the previous error ``E.compile_grammar()``
raised with a new one::

    lrparsing.GrammarError: Conflict: Shift/Reduce and no associativity
    While trying to recognise:
      e.Choice.Prio.Prioritised0 = e ^ '/' e
      e.Choice.Prio.Prioritised0 = e '/' e ^
    on seeing a '/' I could not decide between:
      replace the sequence [e '/' e] with e.Choice.Prio.Prioritised0
    and
      accepting the '/' in the hope it will match:
        e.Choice.Prio.Prioritised0 = e '/' ^ e
    Please remove this ambiguity from your grammar

Firstly, notice how the choices listed don't directly correspond to the
rules in your :class:`Grammar`.  That is because |lrparsing| has compiled
your :class:`Grammar` into :term:`productions <production>`, which is the
only thing an :term:`LR(1) parser <LR(1)>` understands.  You can print
this compiled form using ``print E.repr_productions()`` which yields::

    0   : <E> = START __end_of_input__
    1   : START = e
    2   : e = e.Choice.Prio
          e = /[0-9]+/
    3   : e.Choice.Prio = e.Choice.Prio.Prioritised0
          e.Choice.Prio = e.Choice.Prio.Prioritised1
    4   : e.Choice.Prio.Prioritised0 = e '/' e
    5   : e.Choice.Prio.Prioritised1 = e '+' e

It is worth taking some time to understand how these
:term:`productions <production>` correspond to the original set of
:class:`Rules <Rule>`.  In any case you can now see where all those odd
looking symbol names come from.

Using the same technique as before we deduce the smallest string it
could be looking at and end up with these possibilities:
``e '/' e ^ '/' e``.  In other words it can't decide between these
two :term:`parse trees <parse tree>`::

    e / (e / e)   OR   (e / e) / e

As an aside this is a real problem because integer division isn't
commutative: ``(9 / 3) / 2 = 1``, whereas ``9 / (3 / 2) = 9``.
So if the :term:`parse tree` was being used to evaluate the
expressions, choosing the wrong one would yield an incorrect
answer.

We can't use :class:`Prio` to solve this because the :term:`production`
is conflicting with itself.  The solution is to say division is left
associative::

   class E(Grammar):
       e = Prio(THIS << '/' << THIS, THIS << '+' << THIS) | Token(re="[0-9]+")
       START = e

And with that the parser will compile, and will yield parse trees that
conform to the conventions of arithmetic.

Looking deeper under the hood, an :term:`LR(1) parser <LR(1)>` is called
that because it delays all decisions until it has recognised an entire
:term:`production`.  If you look at the above error messages, you will
see they all involve a :term:`reduce` and that is because is a 
:term:`reduce` is what the :term:`parser` does when it makes the decision
on which :term:`production` it has seen.  There are two ways it can get
confused.

The first involves two :term:`reduces <reduce>`.  Since the :term:`parser`
is making its decision *after* it has seen the entire production, the
only way it can be confused is if two :term:`productions <production>`
have the same :term:`right hand side`.  If you look at the
:term:`Reduce/Reduce conflicts <Reduce/Reduce>` above that is indeed the
case.  But even in that case the :term:`LR(1) parser <LR(1)>` can still
distinguish between the two if they are followed by different
:term:`tokens <token>`.  Consider this :term:`ambiguous`
:class:`Grammar`::

    class F(Grammar):
        a = Token('a')
        b = Token('a')
        START = a + 'x' + 'y' | b + 'x' + 'z'

``F.compile_grammar()`` raises this exception::

    lrparsing.GrammarError: Conflict: Reduce/Reduce
    While trying to recognise state 4:
      a = 'a' ^
      b = 'a' ^
    on seeing a 'x' I could not decide between:
      replace the sequence ['a'] with a
    and
      replace the sequence ['a'] with b
    Please remove this ambiguity from your grammar

Unlike the previous example this :class:`Grammar` isn't inherently
:term:`ambiguous` as there is one unique :term:`parse tree` for every
input.  The problem is even an :term:`LR(1) parser <LR(1)>` isn't
powerful enough to recognise it.  The :term:`symbol` it needs to see
in order to know whether it should be an ``a`` or a ``b`` (the ``'y'``
or ``'z'``) is two :term:`tokens <token>` away and :term:`LR(1)` only
looks one :term:`token` ahead. An LR(2) :term:`parser` would be
powerful enough but we don't have one of those.

In this case it is easy enough re-write the :class:`Grammar` so an
:term:`LR(1) parser <LR(1)>` is powerful enough.  The solution is to
delay recognising ``a`` and ``b`` until the parser can see the ``y``
or ``z``::

    class F(Grammar):
        a = Token('a') + 'x'
        b = Token('a') + 'x'
        START = a + 'y' | b + 'z'

The problem with re-writing like this is it changes the
:term:`parse tree`.  If the back end that processes the
:term:`parse tree` can't tolerate this, something is going to have
to give, and it won't be the :term:`LR(1) parser <LR(1)>`.  Post
processing the :term:`parse tree` so it is in the form the back end
wants is the normal solution.  Console yourself with the knowledge
that people using :term:`LL(1) grammars <LR(1)>` have to do it far
more.

Avoid the temptation to use :class:`Prio` to resolve
:term:`Reduce/Reduce conflicts <Reduce/Reduce>`.  It will make the
:term:`conflict` go away, but it means the :term:`parser` will never
recognise part of your :class:`Grammar`.  In the above example
yielding to temptation and using :class:`Prio` would result in::

    class F(Grammar):
        a = Token('a')
        b = Token('a')
        START = Prio(a + 'x' + 'y', b + 'x' + 'z')

And presto ``F.compile_grammar()`` works.  But now ``F.parse("axz")``
yields::

    lrparsing.ParseError: line 1 column 3: Got 'z' when expecting 'y'

Since ``"axz"`` is a valid input for our :class:`Grammar` this is a bug.
It was expecting a ``'y'`` because our :class:`Grammar` says if it can't
decide choose ``a + 'x' + 'y'`` over ``b + 'x' + 'z'``.  As we know it
could not decide, so will always choose ``a + 'x' + 'y'``.  This means it
can never recognise ``b + 'x' + 'z'``.

Since at least one :term:`reduce` must be involved, the only other
possibility is a :term:`Shift/Reduce conflict <Shift/Reduce>`.  These
always take the form::

     SYMBOL = SYMBOL <anything1> SYMBOL ^
     SYMBOL = SYMBOL ^ <anything2> SYMBOL

You can pick the :term:`Reduce` because it has the :term:`^` at the
end.  In this case the :class:`Grammar` is always :term:`ambiguous`,
which is good because you can fix it by using associativity or
priorities.  The trick in figuring out which to choose is to understand
what the :term:`parser` is trying to decide between, and you can do that
by substituting the :term:`Reduce` :term:`production` into the
:term:`Shift`, like this::

     SYMBOL = SYMBOL <anything1> SYMBOL ^ <anything2> SYMBOL

The :term:`parse trees <parse tree>` become either::

     Shift:  SYMBOL <anything1> (SYMBOL <anything2> SYMBOL)
     Reduce: (SYMBOL <anything1> SYMBOL) <anything2> SYMBOL

If ``<anything1>`` and ``<anything2>`` are different (like ``'*'`` and
``'+'`` above) you can use :class:`Prio` to choose the one that should
happen first.  If they have the same priority (and in particular this 
will be the case if ``<anything1>`` and ``<anything2>`` are the same
thing), then you must use :class:`Left` if the :term:`Shift` is the
correct choice or :class:`Right` if the :term:`Reduce` is the 
correct choice.  If it should never happen use :class:`Nonassoc`.


Examples
========

The source of |lrparsing| contains the follow examples, which can also
be viewed online at 
http://www.stuart.id.au/russell/files/lrparsing/examples:

lrparsing-sqlite3.py
   A :class:`Grammar` for all of the Data Manipulation Statements in
   Sqlite3.py.  |lrparsing| was written because the author could not
   find a Python parsing library that ran at an acceptable speed and
   required less lines of code to do this job than a hand written
   LL(1) parser.

lrparsing-expr.py
   An integer expression evaluator  It illustrates one design pattern
   for building a compiler on top of |lrparsing|: using a
   :class:`Symbols <Symbol>` inbuilt mapping capabilities.  If the job
   is simple this works well.

lua52.py
   A :class:`Grammar` and a cross compiler to Python for Lua 5.2 called.
   It illustrates a second design pattern for building a compiler:
   using a compiler class whose functions have the same names as
   :class:`rules <Rule>` and and :class:`tokens <TokenSymbol>` in the
   :class:`Grammar`.  These functions are then called to compile the
   node as it is output by :func:`parse`.  This is a good way to do it
   for more complex tasks.  It cleanly separates the compiler from
   the parser, while keeping the relationship between the output
   parse tree and the generated code clear.


Acknowledgements
================

|lrparsing| was developed with inspiration from several sources.
The two main ones were parsing.py (http://www.canonware.com/Parsing/)
which from a technical perspective is a powerful and clean LR
parser implementing several things |lrparsing| doesn't, including
a GLR(1) :term:`parser`.  The other source of inspiration is
pyparsing.py (http://pyparsing.wikispaces.com/) which proves that
providing a pythonic interface to parsing is a worthwhile thing
to do.

Finally, thank you to Greg Black for proof reading this document.


LR(1) Glossary
==============

.. glossary::

  Action
      A :term:`action` is a collective name for the two steps an
      :term:`LR(1) parser <parser>` can perform: :term:`shift` and
      :term:`reduce`.  Alternatively, if the :term:`parse table`
      is thought of as a graph, an :term:`action` is the name of
      a line connecting the nodes in the graph.  The name for a
      node in the graph is a :term:`state`.

  Ambiguous
      A parser of any sort has two jobs: to verify the input is valid
      according to the grammar, and to create a :term:`parse tree` for it.
      The words *create a parse tree* could equally well be written
      *assign meaning*.  Eg, ``9 + 3 / 2`` is a valid arithmetic,
      but without the normal rules of arithmetic could have two different
      meanings: ``9 + (3 / 3) = 10`` or ``(9 + 3) / 3 = 4``.  In English,
      jokes are based on this same ambiguity, eg, *Time flies like an
      arrow*.  It follows a parser would not be much use if it could
      not assign a unique meaning to every input sequence.  Sadly it
      is possible (even easy) to write grammars that don't assign a
      unique meaning to every input sequence. These grammars are rejected
      by the parser constructor as :term:`ambiguous`.  Even more sadly no
      practical parser, including :term:`LR(1)`, is powerful enough to
      recognise all non-ambiguous grammars, so they incorrectly say that
      some non-ambiguous grammars are ambiguous.  The only work around
      is to re-write the grammar in a form the parser can use, and post
      process the :term:`parse tree` if the new version is too far
      from the truth.

  Closure
      Closure the name for both an operation performed by
      :term:`item sets <item set>` during their construction and the
      output of that operation, which is a set of :term:`items <item>`.
      An :term:`item set` is a set of :term:`items <item>`, ie,
      partially recognised :term:`productions <production>`.  Initially
      an :term:`item set` contains just its core.  If the next expected
      :term:`symbol` of an :term:`item` of any :term:`item` in the
      :term:`item set` is a :term:`non-terminal`, ie the
      :term:`left hand side` of another :term:`production`, then it
      follows the parser is also recognising a new :term:`item`, which
      is that :term:`production` with its :term:`dot` at the start and
      this new :term:`item` is added to the :term:`closure`.  That new
      :term:`item` could also have a :term:`non-terminal` at its start,
      thus yielding another new :term:`item` recursively.  All items
      added in this way become the :term:`item sets <item set>` closure.

  Core
      The core is the name of the initial subset of :term:`items <item>`
      in an :term:`item set`.  The :term:`core` is constructed from
      the :term:`item sets <item set>` in the
      :term:`state graph <state>` that have lines leading to this new
      :term:`item set`.  It is the :term:`items <item>` in those
      ancestor :term:`item sets <item set>` after the recognition of
      the :term:`symbol` labelling the line connecting them.  The
      :term:`dot` of those items has been moved along one position to
      reflect the recognition of that :term:`symbol`.

  Conflict
  Reduce/Reduce
  Shift/Reduce
      The job of the :term:`parser generator` is to create a graph.
      The nodes of the graph become the parsers :term:`states <state>`.
      The lines that join the nodes of the graph are termed
      :term:`actions <action>` and :term:`gotos <goto>`.  They are
      labeled with the :term:`symbol` the :term:`parser` just
      recognised.  If the :term:`parser generator` can't uniquely
      decide what the next :term:`state` should be when :term:`symbol`
      is seen (ie several lines would be labelled with the same
      :term:`symbol`), the :term:`grammar` is said to have a
      :term:`conflict`.  This means the :term:`grammar` is
      :term:`ambiguous`, because having several possible
      :term:`states <state>` reachable for the same
      :term:`symbol` means there are several possible
      :term:`parse trees <parse tree>`.  For an
      :term:`LR parser <LR(1)>` one of the conflicting
      :term:`actions <action>` will always be a :term:`reduce`
      because an :term:`LR parser <LR(1)>` only makes a decision
      once it has seen an entire production.  Since there are
      two types of :term:`actions <action>`, :term:`shift` and
      :term:`reduce`, this means a :term:`conflict` must be
      between a :term:`shift` and a :term:`reduce` which is
      termed a :term:`Shift/Reduce conflict <Shift/Reduce>`
      or it could be between two :term:`reduces <reduce>` which
      is termed a :term:`Reduce/Reduce conflict <Reduce/Reduce>`.

  Dot
  \^
      Marks a position in the :term:`right hand side` of a production.
      In days gone by when :term:`parse tables <parse table>` were
      constructed manually on paper, a human would mark a dot in a
      production to keep track of where he was up to.  In a computer
      the position of a dot is represented by the number of
      :term:`symbols <symbol>` on the :term:`right hand side` seen
      so far.

  Goto
  Goto Table
      When a :term:`reduce` is performed, the :term:`symbols <symbol>`
      on the :term:`right hand side` of the :term:`production` being
      recognised are popped from the :term:`stack`, thus exposing the
      :term:`production` that contained the :term:`non-terminal` on the
      :term:`left hand side` just recognised.  The act of replacing
      those recognised :term:`symbols <symbol>` with the newly
      recognised :term:`non-terminal` and then moving to the next
      :term:`state` with that :term:`non-terminal` recognised is called
      a :term:`goto`.  The table representing the lines of the
      :term:`state` graph a :term:`state` must move to after accepting
      a :term:`non-terminal` is called the :term:`goto table`.

  Grammar
      Rules that explain how a series of :term:`tokens <token>`
      should be interpreted.  For example, in English
      :term:`tokens <token>` are words and punctuation.  English
      grammar shows how to build up words and punctuation into phrases
      of various kinds (eg, noun phrases, adverb phrases), then the
      phrases into sentences, and the sentences into constructs like
      quotes, paragraphs and chapters.
      The result is often represented as a :term:`parse tree`.

  Input Tuple
      The :term:`parser` recognises :term:`tokens <token>`.  However, it
      is often helpful to have other information associated with the
      :term:`token` that the :term:`parser` doesn't use, such as the
      position in the input stream it was found at for error reporting.
      |lrparsing| calls the combination of a :term:`Token` plus that
      other information an :term:`input tuple`.  |lrparsing| uses a
      Python :func:`tuple <tuple>` with the :term:`Token` as it first
      element for this purpose.

  Item
      A partially recognised :term:`production`.  It is a production,
      plus a marker (the :term:`dot`) which shows how much the parser
      has seen so far, plus the :term:`tokens <token>` that could
      legally follow the :term:`production`.  An LR(0) parser doesn't
      store any such tokens, an :term:`LR(1)` stores all valid tokens
      that could come next and an LR(n) parser stores all valid
      sequences of n tokens that could possibly follow it.  Since an
      :term:`LR(1)` parser only makes decisions after seeing the entire
      :term:`right hand side` of a :term:`production`, it only considers
      the :term:`symbols <symbol>` that could follow a production when
      it does a :term:`reduce`.

  Item Set
      This data structure is the heart of an 
      :term:`LR(1) parser generator <parser generator>`.  As the name
      implies an :term:`item set` is set of :term:`items <item>`.
      Internally this set is broken up into two distinct subsets, the
      :term:`core` and the :term:`closure`.  The generator initialises
      itself by creating the :term:`core` of the initial
      :term:`item set` from the :term:`start symbol`.  This
      :term:`core` consists of the sole :term:`production` of the
      :term:`start symbol` with the :term:`dot` before the first
      :term:`symbol`.  The next step is to compute the :term:`closure`
      of this :term:`core`.  In this case if the first :term:`symbol`
      of the :term:`right hand side` is a :term:`non-terminal`, then it
      follows the parser is also at the start of all the
      :term:`non-terminal's <non-terminal>`
      :term:`productions <production>`, so they are added to the
      :term:`closure`.  And so on, recursively, if they have
      :term:`non-terminal`\s as their first :term:`symbol`.
      So now we have a set of :term:`items <item>` (the :term:`core` and
      its its :term:`closure`), each waiting for their first
      :term:`symbol` to appear.  If the parser receives one such
      :term:`symbol` it can move forward, and what it moves to is a new
      :term:`item set` whose :term:`core` is all the current
      :term:`items <item>` waiting on that :term:`symbol`, but with their
      :term:`dot` position moved forward one step because a new
      :term:`symbol` has been seen.  This process of computing
      :term:`closures <closure>` and generating new
      :term:`item sets <item set>` continues until only duplicate
      :term:`item sets <item set>` are created.  The
      :term:`item sets <item set>` then become the
      :term:`states <state>` of the :term:`parse table`.

  Lhs
  Left Hand Side
      The :class:`Symbol` on the left hand side of a :term:`production`.
      Such symbols are called a :term:`non-terminal`. In a
      |lrparsing| :class:`Grammar` calls the :term:`symbol` on the
      :term:`left hand side` of a :term:`production` a :class:`Rule`.

  LR(1)
      LR(1) is an abbreviation for Left Right (parser with) 1 (symbol
      lookahead).  Left here simply means it treats the string of
      characters like English - it starts reading at the left.  The Right
      here means it delays making a decision on whether it has recognised
      a :term:`production` until it has read the last symbol (right most)
      of its :term:`right hand side`.  The 1 symbol lookahead means it
      considers the next input symbol when it makes this decision.  A
      LR(0) parser doesn't.  If someone were to write an LR(2) parser it
      would consider the next 2 symbols.  The other common type of parser
      is an LL(1) parser, also called a recursive descent parser.  The
      only difference is R becomes an L (for Left), meaning the parser
      must make the decisions on what it is doing using just the 
      :term:`left hand side` of the production it is currently trying to
      recognise and the next input symbol.  In theory because it has
      significantly less information to work with (it hasn't seen any of
      the :term:`right hand side` yet), an LL(1) parser is less powerful
      than a :term:`LR(1)` parser.  In practice recursive descent parsers
      are often written by hand and the programmer cheats by looking ahead
      or re-writing the grammar.  This cheating goes a long way to making
      up the difference in power.  However, automatically generated LL(1)
      parsers don't cheat and so are significantly less powerful (ie,
      LL(1) handles a smaller set of otherwise valid grammars) than
      automatically generated :term:`LR(1)` parsers such as |lrparsing|.

  Parser State
  State
      The state, which is typically identified by a number, is how the
      :term:`LR(1)` parser knows what it is up to.  A :term:`state` is
      called an :term:`item set` by the :term:`parser generator`.  The
      name change reflects a change in the way is is used, not in what
      it is.  Conceptually an :term:`LR(1)` parser is a Deterministic
      Finite Automaton (DFA) which is a fancy name for a way of moving
      through a graph.  The :term:`states <state>` are the nodes in this
      graph.  The :term:`LR(1)` parser moves between nodes in the graph
      by following lines that join them.  There are two ways of doing
      this, collectively termed :term:`actions <action>`.  Every valid
      :term:`token` that can be accepted as input while the parser is
      in that state has an associated :term:`action`.  In fact the only
      time the :term:`parser` raises an error is when it knows the
      input stream can't match the :term:`grammar`, and the only time
      it knows that is when the :term:`token` it just read doesn't
      have an :term:`action` associated with it.  A
      :term:`shift action <shift>` consumes the next :term:`token`
      (pushes it onto the :term:`stack`) and then moves onto the next
      state by following the line labelled with that :term:`token`.  A
      :term:`reduce` action recognises a :term:`production` and so it
      pops the :term:`stack` to see what parent :term:`state` the
      :term:`LR(1) parser <parser>` is trying to recognise.  That
      parent state will have a line in the graph labeled with the
      :term:`non-terminal` the :term:`left hand side` of the
      :term:`production` just recognised, which will take it to the
      next :term:`state`.  People familiar with the purest form of
      regular expressions will know that they can also be recognised
      with DFA's.  An :term:`LR(1)` parser is a souped up version of
      a regular expression DFA that recognises the
      :term:`right hand side` of its productions.

  Parse Table
      A parse table is the data structure that drives a :term:`parser`.
      The parse table is an array of :term:`states <state>`, the
      :term:`states <state>` being identified by their array index.
      The :term:`parse table` can be viewed as the description of the
      nodes and lines of a graph.  The nodes of the graph are the
      :term:`states <state>`.  The lines are the :term:`state` the
      :term:`parser` moves to when it recognises a :term:`symbol`,
      and thus are labelled with that :term:`symbol`.  The lines 
      correspond to the state :term:`actions <action>` and
      :term:`gotos <goto>`.

  Parse Tree
      The way a parser explains the meaning of its input is to output a
      parse tree.  The parse tree assigns the meaning to the
      :term:`symbols <symbol>` and the relationships between them.  The
      sentence *Time flies like an arrow* can be parsed two ways.  In
      the one way "Time" is an adjective describing a type of insect,
      and "like" is a verb describing what "Time flies" do to arrows.
      An |lrparsing| parse tree for that meaning might look like::

        (sentence
          (noun_phrase (adjective 'Time') (noun 'flies'))
          (verb_phrase
            (verb 'like')
            (noun_phrase (determiner 'an') (noun 'arrow')))))

      The alternative, where "Time" is a noun, looks like this::

        (sentence
          (noun 'Time')
          (verb_phrase
            (verb 'flies')
            (preposition_phrase 
              (preposition 'like')
              (noun_phrase (determiner 'an') (noun 'arrow')))))

  Parser
      The parser in an :term:`LR(1)` is a simple loop that processes a
      stream of :term:`input tuples <input tuple>` according to a
      :term:`parse table`, and produces a :term:`parse tree` as a
      consequence.  The same :term:`parser` is used for all input
      streams and all :term:`parse tables <parse table>`.  It is fast
      because it is simple.  Its one internal data structure is the
      :term:`stack`.  The :term:`parse table` is actually a graph,
      and the :term:`parser's <parser>` sole job is move to a new node
      in the graph as it recognises each new :term:`symbol` by following
      the line labelled with that :term:`symbol`.  As it goes it outputs
      the :term:`productions <production>` it has recognised, and these
      become the :term:`parse tree`.

  Parser Generator
      A parser generator takes a :term:`grammar` as input and generates
      a :term:`parse table` as output, unless the :term:`grammar` is
      :term:`ambiguous` in which cause it raises an error.  It does this
      by constructing :term:`item sets <item set>`.

  Production
      The :term:`LR(1)` name for the lines describing a :term:`Grammar` an
      :term:`LR(1) parser generator <parser generator>` accepts as input.
      |lrparsing| does not use :term:`productions <production>`, it uses
      :class:`rules <Rule>`.  It compiles your :class:`rules <Rule>` into
      :term:`LR(1)` :term:`productions <production>` for you.  You can
      display those productions using :func:`repr_productions`.
      Productions are similar to :class:`rules <Rule>` in that they have
      a :term:`left hand side` which replaces the symbols on the
      :term:`right hand side` when they are recognised.  The differences
      between a :class:`Rule` and a :term:`production` is a
      :term:`production` can only be a :class:`Sequence`, and alternative
      derivations for the same :term:`non-terminal` are expressed by
      allowing a :term:`non-terminal` to have many
      :term:`productions <production>`.

  Non-Terminal
      A symbol that appears on the :term:`left hand side` of a
      production.  In |lrparsing| :class:`Grammars <Grammar>` the
      :class:`Rules <Rule>` are its
      :term:`non-terminals <non-terminal>`.

  Reduce
      When the symbols on the right hand side of a production are seen
      they may be replaced by the :term:`non-terminal` on the left hand
      side.  The action is called a :term:`reduce`.  The parser executes
      the :term:`reduce` by popping recognised :term:`symbols <symbol>`
      (ie, the :term:`symbols <symbol>` on the :term:`right hand side` of
      the matched :term:`production`) from the :term:`stack`, pushing
      the :term:`non-terminal` on the :term:`left hand side` of the
      :term:`production` onto the :term:`stack`, then executing a
      :term:`goto`.

  Rhs
  Right Hand Side
      The sequence of symbols on the right hand side of a
      :term:`production`.  When these symbols are seen in the order they
      were given, they can be replaced on the :term:`stack` (via an
      operation called a :term:`reduce`) by the 
      :term:`non-terminal` on the :term:`left hand side`.

  Shift
      When a :term:`token` is recognised as valid it is removed from the
      input stream and stored away.  This action is called a shift.  The
      :term:`parser` executes the shift by pushing the recognised
      :term:`token` onto the :term:`stack` and moving to a new
      :term:`state`.

  Stack
      The stack is a data structure the :term:`LR(1) parser <parser>`
      uses internally as it does its thing.  A :term:`shift` pushes
      the next input :class:`Token` onto the stack.  A :term:`reduce`
      happens when the :term:`symbols <symbol>` on the
      :term:`right hand side` of a :term:`production` match the
      symbols on the top of the stack.  When they do, a
      :term:`reduce` replaces those matching symbols with the 
      :term:`non-terminal` on the :term:`left hand side` of the
      :term:`production`.  That :term:`symbol` will always (unless it
      is the :term:`start symbol`) be part of another production that
      the parser was in the process of recognising, and now its
      (probably partial) :term:`right hand side` will be what is now
      on the top of the stack.  The stack continues downwards with
      :term:`symbols <symbol>` from partially recognised
      :term:`productions <production>` until, at its bottom, we find
      the partially recognised production from the :term:`start symbol`.
      The :term:`Symbols <symbol>` popped from the :term:`stack`
      become nodes in the :term:`parse tree`.  Thus the :term:`stack`
      is actually a partially constructed branch of the
      :term:`parse tree`, with the bottom becoming the root node of
      the :term:`parse tree`.

  Start Symbol
      The goal of the grammar is to recognise this symbol.  Although
      an |lrparsing| :class:`Grammar` requires a :class:`Symbol` named
      :attr:`Grammar.START` and its goal is indeed to recognise that
      symbol, it isn't the real :term:`start symbol`.  The real
      :term:`start symbol` has a single :term:`production` whose
      :term:`right hand side` is the :attr:`Grammar.START` symbol
      followed by an internal :class:`MetaToken` called
      ``__end_of_input__``.  It is returned by :func:`epoch_symbol`.

  Symbol
      In :term:`LR(1)` parlance a symbol is the name for something that
      is recognised by the :term:`parser`, ie, it is a collective name
      for :term:`tokens <token>` and
      :term:`non-terminals <non-terminal>`.

  Terminal
      What |lrparsing| calls a :term:`token`.

  Token
      Typically what the :term:`parser` is trying to recognise is a
      series of characters.  The first step is to break that series of
      characters into :term:`tokens <token>`.  In English a
      :term:`token` is a word, or a punctuation character.  The
      :term:`parser` then works on these :term:`tokens <token>`,
      not the individual characters.

  Tokeniser
      The thing that turns text into a stream of :term:`Tokens <Token>`.

.. rubric:: Footnotes

.. [#grammar_rule_assignment]

    Assigning a :class:`Symbol` instance to a rule in a :class:`Grammar`
    subclass does not work like normal Python assignment, where the left
    hand side and the right hand side are the same objects after the
    assignment.  Instead the :class:`Grammar` subclass creates a new
    :class:`Rule` object to hold the :class:`Symbol` object on the
    right hand side which results in some unusual side effects::

        class MyGrammar(Grammar):
            rule1 = Token('a')          # Legal, but rule1 != Token('a')
            rule2 = rule1 + Token('b')  # Also legal
            rule3 = rule2               # Illegal, direct assignment
            rule3 = rule2 * 1           # Same result but is legal
            START = rule3               # Legal because this is START

.. [#grammar_underscore_example]

    Not outputting rules whose name starts with a leading underscore to
    the :term:`parse tree` means these two grammars produce the same
    :term:`parse tree`::

        class Ex1(Grammar):
            _rule1 = Token('x')
            START = _rule1 + Token("y")

        class Ex2(Grammar):
            START = Token('x') + Token("y")

.. [#symbol_example]

    An example of syntactic sugar for :class:`Symbol`.
    This::

        class MyGrammar(Grammar):
            rule = Choice(Token('x'), Prio(Repeat(Token('a'), 1, 1), Sequence(Opt(Token('y')), Token('z'))))

    Can be rewritten as this::

        class MyGrammar(Grammar):
            rule = Token('x') | (Token('a') * 1, Opt('y') + 'z')

.. [#token_registry_example]

    An example of using a :class:`TokenRegistry`::

        class MyGrammar(Grammar):
            class MyTokens(TokenRegistry):
                my_token = UserToken()
                identifier = Token(re='[A-Za-z_][A-Za-z_0-9]*', case=False)
                #
                # Being a subclass of Symbol, TokenSymbols have dicts.
                # Since a Grammar subclasses, changes to a TokenSymbol's
                # .dict are ignored, it is best to put them here.
                #
                my_token["a"] = "a_value"
            START = MyTokens.my_token | MyTokens.identifier
        assert MyGrammar.MyTokens.my_token == 'MyTokens.my_token'

.. [#token_registry_corollary]

    A corollary of a :class:`TokenRegistry` being the only way to obtain
    a handle to the :class:`TokenSymbol` used by the parser is if you
    use :class:`UserTokens <UserToken>`, you have to use a
    :class:`TokenRegistry` otherwise there is no way to pass the correct
    :class:`UserToken` instance to the parser.

.. [#tokeniser_chain]

    The inbuilt tokeniser passes :term:`input tuples <input tuple>`
    through to the :func:`parser <parse>` so you can use it to handle
    the bulk of the work even if it can't handle all of it.  Write
    a generator that replaces the input :func:`strings <str>` the
    inbuilt parser can't handle with :term:`input tulpes <input tuple>`
    and yields the remainder of the input untouched.  Pass your
    generator to :func:`parse` and you are done.

.. [#tokeniser-override]
    
    If you want to intercept the :term:`input tuples <input tuple>`
    emitted by the :ref:`inbuilt tokeniser <inbuilt-tokeniser>`,
    perhaps to change or delete them, override
    :meth:`TokenRegistry.tokeniser` like this::

        class G(Grammar):
            class T(TokenRegistry):
                def tokeniser(self, *args):
                    for input_tuple in super(G.T, self).tokeniser(*args):
                        # do something with input_tuple, maybe
                        yield input_tuple


.. [#tokens]

    Using :term:`tokens <token>` rather than dealing with
    :func:`strings <str>` directly is primarily an optimisation.  Being
    more powerful than a regular expression an :term:`LR(1)` parser could
    easily do what the :term:`tokeniser` does: recognise identifiers,
    numbers, keywords and so in a character stream.  But that power comes
    at a price.  An :term:`LR(1)` parser both consumes more memory and is
    slower than a regular expression.  Since :term:`tokens <token>` are
    typically 4 or 5 characters long we reduce the workload on an
    :term:`LR(1)` parser by a factor of 4 or 5 by getting a light weight
    regular expression to break the input stream up into
    :term:`tokens <token>`, and then have the
    :term:`LR(1) parser <parser>` operate on them.

.. [#error-recovery-lie]

    The statement that the :term:`stack` passed to :func:`on_error` and
    :exc:`ParseError` is a :class:`Lr1State` is a simplification.  It
    will be an :class:`Lr1State` if :func:`pre_compile_grammar` is used,
    otherwise it will be an :term:`item set`.  However, :class:`Lr1State`
    contains a strict subset of the information in an :term:`item set`
    so the description applies to both.

.. |lrparsing| replace:: :mod:`lrparsing`
