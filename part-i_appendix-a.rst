.. highlight:: erlang
   :linenothreshold: 3

*********************
附录A Erlang 语法参考
*********************

这部分语法参考是 LALR 语法的改编版本。

此语法和严格的 LALR 语法对 ``match_expr`` 有不同理解。 ``match_expr`` 中等号左边可以是一个模式或者表达式， Erlang 编译器会在语义分析时确定其含义。

.. list-table::
    :widths: 30 30 40
    :header-rows: 1

    * - 类型
      - 优先级
      - 运算符
    * - Nonassoc
      - 0
      - ``'catch'.``
    * - Right
      - 200
      - ``'='.``
    * - Right
      - 200
      - ``'!'.``
    * - Left
      - 300
      - ``add op.``
    * - Left
      - 400
      - ``mult op.``
    * - Nonassoc
      - 500
      - ``prefix op.``

--------

.. list-table::
    :widths: 10 20 70
    :header-rows: 1

    * - 编号
      - 非终结符
      - 表达式
    * - 1
      - ``add op``
      - ``:= "+" | "-" | "bor" | "bxor" | "bsl" | "bsr"``
    * - 2
      - ``comp_op``
      - ``:= "==" | "/=" | "=<" | "<" | ">=" | ">" | "=:=" | "=/="``
    * - 3
      - ``mult_op``
      - ``:= "*" | "/" | "div" | "rem" | "band"``
    * - 4
      - ``prefix_op``
      - ``:= "+" | "-" | "bnot"``
    * - 5
      - ``basic_type``
      - ``:= "atom" | "number" | "string" | "var" | "true"``
    * - 6
      - ``pattern``
      - ``:= basic_type | pattern_list | pattern_tuple``
    * - 7
      - ``pattern_list``
      - ``:= "[" "]" | "[" pattern pattern tail "]"``
    * - 8
      - ``pattern_tail``
      - ``:= "|" pattern | "," pattern pattern_tail | ε``
    * - 9
      - ``pattern_tuple``
      - ``:= "{" "}" | "{" patterns "}"``
    * - 10
      - ``patterns``
      - ``:= pattern | pattern "," patterns``
    * - 11
      - ``expr``
      - ``:= basic_type | list | tuple | function_call | expr add op expr | expr mult_op expr | prefix_op expr | "(" expr ")" | "begin" exprs "end" | "catch" expr | case_expr | if_expr | receive_expr | match_expr | send expr``
    * - 12
      - ``list``
      - ``:= "[" "]" | "[" expr expr_tail "]"``
    * - 13
      - ``expr_tail``
      - ``:= "|" expr | "," expr expr_tail | ε``
    * - 14
      - ``tuple``
      - ``:= "{" "}" | "{" exprs "}"``
    * - 15
      - ``function_call``
      - ``:= "atom" "(" parameter_list ")" | "atom" ":" "atom" "(" parameter_list ")"``
    * - 16
      - ``parameter_list``
      - ``:= exprs | ε``
    * - 17
      - ``case_expr``
      - ``:= "case" expr "of" cr_clauses "end"``
    * - 18
      - ``cr_clause``
      - ``:= pattern clause_guard clause_body``
    * - 19
      - ``cr_clauses``
      - ``:= cr_clause | cr_clause ";" cr_clauses``
    * - 20
      - ``if_expr``
      - ``:= "if" if_clauses "end"``
    * - 21
      - ``if_clause``
      - ``:= guard clause_body``
    * - 22
      - ``if_clauses``
      - ``:= if_clause | if_clause ";" if_clauses``
    * - 23
      - ``receive_expr``
      - ``:= "receive" "after" expr clause_body "end" | "receive" cr_clauses "end" | "receive" cr_clauses "after" expr clause_body "end"``
    * - 24
      - ``match_expr``
      - ``:= expr "=" expr``
    * - 25
      - ``send expr``
      - ``:= expr "!" expr``
    * - 26
      - ``exprs``
      - ``:= expr | expr "," exprs``
    * - 27
      - ``guard_expr``
      - ``:= basic_type | guard_expr_list | guard_expr_tuple | guard_call | "(" guard_expr ")" | guard_expr add op guard_expr | guard_expr mult_op guard_expr | prefix_op guard_expr``
    * - 28
      - ``guard_expr_list``
      - ``:= "[" "]" | "[" guard_expr guard_expr_tail "]"``
    * - 29
      - ``guard_expr_tail``
      - ``:= "|" guard_expr | "," guard_expr guard_expr_tail | ε``
    * - 30
      - ``guard_expr_tuple``
      - ``:= "{" "}" | "{" guard_exprs "}"``
    * - 31
      - ``guard_exprs``
      - ``:= guard_expr | guard_expr "," guard_exprs``
    * - 32
      - ``guard_call``
      - ``:= "atom" "(" guard_parameter_list ")"``
    * - 33
      - ``guard_parameter_list``
      - ``:= guard_exprs | ε``
    * - 34
      - ``bif_test``
      - ``:= "atom" "(" guard_parameter_list ")"``
    * - 35
      - ``guard_test``
      - ``:= bif_test | guard_expr comp_op guard_expr``
    * - 36
      - ``guard_tests``
      - ``:= guard_test | guard_test "," guard_tests``
    * - 37
      - ``guard``
      - ``:= "true" | guard_tests``
    * - 38
      - ``function_clause``
      - ``:= clause_head clause_guard clause_body``
    * - 39
      - ``clause_head``
      - ``:= "atom" "(" formal parameter_list ")"``
    * - 40
      - ``formal parameter_list``
      - ``:= patterns | ε``
    * - 41
      - ``clause_guard``
      - ``:= "when" guard | ε``
    * - 42
      - ``clause_body``
      - ``:= "->" exprs``
    * - 43
      - ``function``
      - ``:= function_clause | function_clause ";" function``
    * - 44
      - ``attribute``
      - ``:= pattern | "[" farity_list "]" | "atom" "," "[" farity_list "]"``
    * - 45
      - ``farity_list``
      - ``:= farity | farity "," farity_list``
    * - 46
      - ``farity``
      - ``:= "atom" "/" "number"``
    * - 47
      - ``form``
      - ``:= "-" "atom" "(" attribute ")" | function``

--------

.. csv-table::
    :header-rows: 1

    非终结符, 编号
    ``add_op``, \*1 11 27
    ``attribute``, \*44 47
    ``basic_type``, \*5 6 11 27
    ``bif_test``, \*34 35
    ``case_expr``, 11 \*17
    ``clause_body``, 18 21 23 38 \*42
    ``clause_guard``, 18 38 \*41
    ``clause_head``, 38 \*39
    ``comp_op``, \*2 35
    ``cr_clause``, \*18 19
    ``cr_clauses``, 17 \*19 19 23
    ``expr``, \*11 11 12 13 17 23 24 25 26
    ``expr_tail``, 12 \*13 13
    ``exprs``, 11 14 16 \*26 26 42
    ``farity``, 45 \*46
    ``farity_list``, 44 \*45 45
    ``form``, \*47
    ``formal_parameter_list``, 39 \*40
    ``function``, \*43 43 47
    ``function_call``, 11 \*15
    ``function_clause``, \*38 43
    ``guard``, 21 \*37 41
    ``guard_call``, 27 \*32
    ``guard_expr``, \*27 27 28 29 31 35
    ``guard_expr_list``, 27 \*28
    ``guard_expr_tail``, 28 \*29 29
    ``guard_expr_tuple``, 27 \*30
    ``guard_exprs``, 30 \*31 31 33
    ``guard_parameter_list``, 32 \*33 34
    ``guard_test``, \*35 36
    ``guard_tests``, \*36 36 37
    ``if_clause``, \*21 22
    ``if_clauses``, 20 \*22 22
    ``if_expr``, 11 \*20
    ``list``, 11 \*12
    ``match_expr``, 11 \*24
    ``mult_op``, \*3 11 27
    ``parameter_list``, 15 \*16
    ``pattern``, \*6 7 8 10 18 44
    ``pattern_list``, 6 \*7
    ``pattern_tail``, 7 \*8 8
    ``pattern_tuple``, 6 \*9
    ``patterns``, 9 \*10 10 40
    ``prefix_op``,  \*4 11 27
    ``receive_expr``, 11 \*23
    ``send_expr``, 11 \*25
    ``tuple``, 11 \*14

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
