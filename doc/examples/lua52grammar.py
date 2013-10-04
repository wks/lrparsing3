from lrparsing import (
        Grammar, Keyword, List, Opt, Prio, Repeat, Ref, Right,
        THIS, Token, Tokens, TokenRegistry)

#
# The complete Lua52 grammar.
#
class Lua52Grammar(Grammar):
    class T(TokenRegistry):
        name = Token(re='[a-zA-Z_][a-zA-Z_0-9]*')
        short_string = Token(re=r'"(?:[^"\\]|\\.)*"' + "|" + r"'(?:[^'\\]|\\.)*'")
        long_string = Token(re=r'\[(=*)\[.*?\\]\1\]')
        decimal_number = Token(re=
            '(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)' +
            '(?:[Ee][+-]?[0-9]+)?')
        hex_number = Token(re=
            '0[xX]' +
            '(?:[0-9a-fA-Z]+(?:[.][0-9a-fA-F]*)?|[.][0-9a-fA-F]+)' +
            '(?:[pP][+-]?[0-9a-zA-F]+)?')
        var_args = Token("...")
        line_comment = Token(re='--(?:\n|(?:[^[\n]|\\[=*[^[=])[^\n]*\n)')
        block_comment = Token(re=r'--\[(=*)\[.*?\]\1\]')
    #
    # Forward declarations.
    #
    exp = Ref("exp")
    prefix_exp = Ref("prefix_exp")
    last_statement = Ref("last_statement")
    statement = Ref("statement")
    #
    # Collective tokens.
    #
    string = T.short_string | T.long_string
    number = T.decimal_number | T.hex_number
    variable_ref = List(T.name, ".", min=1)
    subscript_exp = prefix_exp + '[' + exp + ']'
    var = variable_ref | subscript_exp
    var_list = List(var, ',', min=1)
    exp_list = List(exp, ',', min=1)
    #
    # Table constructor.
    #
    field = '[' + exp + ']' + '=' + exp | T.name + '=' + exp | exp
    table_constructor = '{' + List(field, Tokens(", ;"), opt=True) + '}'
    #
    # A function call.
    #
    function_args =  (
            '(' + Opt(exp_list) + ')' | table_constructor | string)
    function_call =  (
            prefix_exp + function_args |
            prefix_exp + ':' + T.name + function_args)
    #
    # A sequnece of statements.
    #
    block = Repeat(statement + Opt(';')) + Opt(last_statement + Opt(';'))
    #
    # Scope control.  These 0 length productions create and delete scopes
    # for the variables used and declared between them.
    #
    begin_scope = THIS * 0
    end_scope = THIS * 0
    loop = THIS * 0
    begin_loop_scope = begin_scope + loop
    scope = begin_scope + block + end_scope
    loop_scope = begin_loop_scope + block + end_scope
    #
    # A Function definition.
    #
    name_list = Right(List(T.name, ',', min=1))
    _parameter_list = T.name | T.var_args | T.name + ',' + THIS
    parameter_list = Opt(_parameter_list)
    function_body = (
            begin_scope + '(' + parameter_list + ')' +
            block + end_scope + Keyword('end'))
    anon_function = Keyword('function') + function_body
    #
    # An expression.
    #
    constant = Tokens("", 'nil false true')
    adjusted_exp = '(' + exp + ')'
    _atom = (
        constant | number | string | T.var_args | anon_function |
        table_constructor)
    prefix_exp = Prio(function_call, adjusted_exp, var)
    exp = Prio(
        _atom,
        prefix_exp,
        exp >> '^' >> exp,
        Tokens("- #", "not") >> exp,
        exp << Tokens("* / %") << exp,
        exp >> ".." >> exp,
        exp << Tokens("+ -") << exp,
        exp << Tokens("< <= > >= == ~=") << exp,
        exp << Keyword("and") << exp,
        exp << Keyword("or") << exp)
    function_name = variable_ref + Opt(':' + T.name)
    #
    # The statements.
    #
    assign_st = var_list + '=' + exp_list
    do_st = Keyword('do') + scope +  Keyword('end')
    while_st = (
            Keyword('while') + exp + Keyword('do') +
            loop_scope + Keyword('end'))
    repeat_st = Keyword('repeat') + loop_scope + Keyword('until') + exp
    if_st = (
        Keyword('if') + exp + Keyword('then') + scope +
        Repeat(Keyword('elseif') + exp + Keyword('then') + scope) +
        Opt(Keyword('else') + scope) +
        Keyword('end'))
    for_steps = T.name + '=' + exp + ',' + exp + Opt(',' + exp)
    for_step_st = (
        Keyword('for') + begin_loop_scope + for_steps +
        Keyword('do') + block + end_scope + Keyword('end'))
    for_name_list = name_list * 1
    for_in_st = (
        Keyword('for') + begin_loop_scope + for_name_list + Keyword('in') +
        exp_list + Keyword('do') + block + end_scope + Keyword('end'))
    function_call_st = function_call * 1
    function_decl_st = Keyword('function') + function_name + function_body
    local_function_decl_st = (
        Keyword('local') + Keyword('function') + T.name + function_body)
    local_assign_st = Keyword('local') + name_list + Opt('=' + exp_list)
    statement =  (
        assign_st |
        do_st |
        for_in_st |
        for_step_st |
        function_call_st |
        function_decl_st |
        if_st |
        local_assign_st |
        local_function_decl_st |
        repeat_st |
        while_st)
    return_st = Keyword('return') + Opt(exp_list)
    break_st = Keyword("break")
    last_statement = return_st | break_st
    #
    # Special grammar symbols.
    #
    begin_program = begin_scope * 1
    end_program = end_scope * 1
    START = begin_program + block + end_program
    COMMENTS = T.line_comment | T.block_comment

if __name__=='__main__':
    print(Lua52Grammar.pre_compile_grammar())


