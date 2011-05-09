**************
第4章 使用元组
**************

:翻译: 王飞
:校订: 连城

元组用以将多个对象组合成一个新的复杂对象。对象\ ``{E1,E2,E3,...En}``\ 表示一个\ **大小为 n 的元组**\ 。元组用于描述包含\ **固定**\ 数目的元素的数据结构；对于\ **可变**\ 数目的元素，应该使用\ **列表**\ 来存储。

处理元组的BIF
=============

以下是一些可以用来操纵元组的BIF：

``tuple_to_list(T)``

    将元组\ ``T``\ 转化成一个列表。

    如：\ ``tuple_to_list({1,2,3,4})``\ |=>|\ ``[1,2,3,4]``\ 。

``list_to_tuple(L)``

    将列表\ ``L``\ 转化成一个元组。

    如：\ ``list_to_tuple([a,b,c])``\ |=>|\ ``{a,b,c}``\ 。

``element(N, T)``

    返回元组\ ``T``\ 的第\ ``N``\ 个元素。

    如：\ ``element(3,{a,b,c,d})``\ |=>|\ ``c``\ 。

``setelement(N, T, Val)``

    返回一个新的元组，这个元组是将元组\ ``T``\ 的第\ ``N``\ 个元素用\ ``Val``\ 替换之后的一个拷贝。

    如：\ ``setelement(3, {a,b,c,d}, xx)``\ |=>|\ ``{a,b,xx,d}``\ 。

``size(T)``

    返回元组\ ``T``\ 包含的元素个数。

    如：\ ``size({a,b,c})``\ |=>|\ ``3`` 。

返回多个值
==========

我们经常想让一个函数返回多个值，使用元组来实现这一目的是十分方便的。

例如，函数\ ``parse_int(List)``\ 从一个由ASCII字符构成的列表\ ``List``\ 中提取最开始的数字，如果存在，就返回一个由被提取出来的数字和列表剩下的部分组成的元组，如果列表中没有数字的话，就返回原子式\ ``eoString``\ 。

.. code-block:: erlang

    parse_int(List) ->
        parse_int(skip_to_int(List), 0).

    parse_int([H|T], N) when H >= $0, H =< $9 ->
        parse_int(T, 10 * N + H - $0);
    parse_int([], 0) ->
        eoString;
    parse_int(L, N) ->
        {N,L}.

``skip_to_int(L)``\ 返回\ ``L``\ 中第一个以ASCII字符\ ``0``\ 到\ ``9``\ 中的任意一个开始的子列表。

.. code-block:: erlang

    skip_to_int([]) ->
        [];
    skip_to_int([H|T]) when H >= $0, H =< $9 ->
        [H|T];
    skip_to_int([H|T]) ->
        skip_to_int(T).

如果我们使用字符串\ ``"abcd123def"``\ （\ ``"abcd123def"``\ 的列表形式是\ ``[97,98,99,49,50,51,100,101,102]``\ ）来测试\ ``parse_int``\ ：

.. code-block:: erlang

    > tuples:parse_int("abc123def").
    {123,[100,101,102]}}

在\ ``parse_int``\ 的基础上，可以实现一个提取所有嵌入在字符串里面的数字的解释器。

.. code-block:: erlang

    parse_ints([]) ->
        [];
    parse_ints(L) ->
        case parse_int(L) of
            eoString ->
                [];
            {H,Rest} ->
                [H|parse_ints(Rest)]
        end.

因此：

.. code-block:: erlang

    > tuples:parse_ints("abc,123,def,456,xx").
    [123,456]

密码加密
========

几乎每天笔者们都不得不记住许多不同的密码——信用卡的密码，门禁密码等等。这些密码可以用一种方法记录下来，并且不会被犯罪分子利用吗？

假设我们有一张密码为\ ``3451``\ 的LISA信用卡，它的密码可以像这样被编码：
::

    a b c d e f g h i j k l m n o p q r s t u v w x y z
    1 0 5 3 4 3 2 7 2 5 4 1 9 4 9 6 3 4 1 4 1 2 7 8 5 0   lisa

这样密码就可以写在一张纸上，即使这张纸落在他人手上，密码也是安全的。

我们如何解码信息呢？用来加密密码的密钥是公开的——因此我们可以很容易地读出密码（3451）--试试看！

我们很容易的就可以构造一个用来执行加密的函数\ ``encode(Pin, Password)``\ [#]_\ ：

.. code-block:: erlang

    encode(Pin, Password) ->
        Code = {nil,nil,nil,nil,nil,nil,nil,nil,nil,
                nil,nil,nil,nil,nil,nil,nil,nil,nil,
                nil,nil,nil,nil,nil,nil,nil,nil},
        encode(Pin, Password, Code).

    encode([], _, Code) ->
        Code;
    encode(Pin, [], Code) ->
        io:format("Out of Letters~n",[]);

    encode([H|T], [Letter|T1], Code) ->
        Arg = index(Letter) + 1,
        case element(Arg, Code) of
            nil ->
                encode(T, T1, setelement(Arg, Code, index(H)));
            _ ->
                encode([H|T], T1, Code)
        end.

    index(X) when X >= $0, X =< $9 ->
        X - $0;

    index(X) when X >= $A, X =< $Z ->
        X - $A.

我们看一下以下的例子：

.. code-block:: erlang

    > pin:encode("3451","DECLARATIVE").
    {nil,nil,5,3,4,nil,nil,nil,nil,nil,nil,1,nil,nil,nil,
     nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil}

我们现在使用随机数来替换没有被填充的\ ``nil``\ 元素：

.. code-block::  erlang

    print_code([], Seed) ->
        Seed;

    print_code([nil|T], Seed) ->
        NewSeed = ran(Seed),
        Digit = NewSeed rem 10,
        io:format("~w ",[Digit]),
        print_code(T, NewSeed);

    print_code([H|T],Seed) ->
        io:format("~w ",[H]),
    print_code(T, Seed).

    ran(Seed) ->
        (125 * Seed + 1) rem 4096.

然后我们需要一些小函数将所有东西连接在一起：

.. code-block:: erlang

    test() ->
        title(),
        Password = "DECLARATIVE",
        entries([{"3451",Password,lisa},
                 {"1234",Password,carwash},
                 {"4321",Password,bigbank},
                 {"7568",Password,doorcode1},
                 {"8832",Password,doorcode2},
                 {"4278",Password,cashcard},
                 {"4278",Password,chequecard}]).

    title() ->
        io:format("a b c d e f g h i j k l m \
                   n o p q r s t u v w x y z~n",[]).

    entries(List) ->
        {_,_,Seed} = time(),
        entries(List, Seed).

    entries([], _) -> true;

    entries([{Pin,Password,Title}|T], Seed) ->
        Code = encode(Pin, Password),
        NewSeed = print_code(tuple_to_list(Code), Seed),
        io:format(" ~w~n",[Title]),
        entries(T, NewSeed).

最后我们可以运行这个程序了：

.. code-block:: erlang

    1> pin:test().
    a b c d e f g h i j k l m n o p q r s t u v w x y z
    1 0 5 3 4 3 2 7 2 5 4 1 9 4 9 6 3 4 1 4 1 2 7 8 5 0 lisa
    9 0 3 1 2 5 8 3 6 7 0 4 5 2 3 4 7 6 9 4 9 2 7 4 9 2 carwash
    7 2 2 4 3 1 2 1 8 3 0 1 5 4 1 0 5 6 5 4 3 0 3 8 5 8 bigbank
    1 0 6 7 5 7 6 9 4 5 4 8 3 2 1 0 7 6 1 4 9 6 5 8 3 4 doorcode1
    1 4 3 8 8 3 2 5 6 1 4 2 7 2 9 4 5 2 3 6 9 4 3 2 5 8 doorcode2
    7 4 7 4 2 5 6 5 8 5 8 8 9 4 7 6 5 0 1 2 9 0 9 6 3 8 cashcard
    7 4 7 4 2 7 8 7 4 3 8 8 9 6 3 8 5 2 1 4 1 2 1 4 3 4 chequecard
    true

之后这些信息可以用很小的字体打印出来，粘在一张邮票的背后，藏在你的领带里面\ [#]_\ 。

字典
====

我们将一组键惟一的\ ``键—值``\ （\ ``Key-Value``\ ）对定义为字典\ [#]_\ 。存在字典里的值可能会重复。对\ ``Key``\ 和\ ``Value``\ 的数据类型都没有限制，但是只能通过\ ``Key``\ 来查询字典。

我们定义一下字典操作：

``new()``

    创建并返回一个空字典。

``lookup(Key, Dict)``

    在字典\ ``Dict``\ 中查找一个\ ``Key-Value``\ 对，如果找到则返回\ ``{value, Value}``\ ，否则返回\ ``undefined``\ 。

``add(Key, Value, Dict)``

    添加一个新的\ ``Key-Value``\ 对到字典\ ``Dict``\ 中，并返回一个新的字典，以反映\ ``add``\ 函数对字典造成的改变。

``delete(Key, Dict)``

    从字典\ ``Dict``\ 里删除\ ``Key``\ 所对应的\ ``Key-Value``\ 对，并返回一个新的字典。

程序4.1展示了一个字典是怎样将\ ``Key-Value``\ 对以元组的形式存放到列表里面的。它并不是实现一个字典最好的方法，在这里它只是一个例子。

.. topic:: 程序4.1

    .. code-block:: erlang

        -module(dictionary).
        -export([new/0,lookup/2,add/3,delete/2]).

        new() ->
            [].

        lookup(Key, [{Key,Value}|Rest]) ->
            {value,Value};

        lookup(Key, [Pair|Rest]) ->
            lookup(Key, Rest);

        lookup(Key, []) ->
            undefined.

        add(Key, Value, Dict) ->
            NewDict = delete(Key, Dict),
            [{Key,Value}|NewDict].

        delete(Key, [{Key,Value}|Rest]) ->
            Rest;

        delete(Key, [Pair|Rest]) ->
            [Pair|delete(Key, Rest)];
        delete(Key, []) ->
            [].

我们用\ ``字典``\ 来构建和管理一个包含了各位作者鞋码的小型数据库：

.. code-block:: erlang

    D0 = dictionary:new().
    []
    > D1 = dictionary:add(joe, 42, D0).
    [{joe,42}]
    > D2 = dictionary:add(mike, 41, D1).
    [{mike,41},{joe,42}]
    > D3 = dictionary:add(robert, 43, D2).
    [{robert,43},{mike,41},{joe,42}]
    > dictionary:lookup(joe, D3).
    {value,42}
    > dictionary:lookup(helen, D3).
    undefined
    ...

非平衡二叉树
============

字典适合保存少量的数据项，但是当项的数量不断增加，更好的方法是用通过使用键的序列关系来访问数据的树形结构来组织数据。这种结构的访问时间与它所包含的项的数量成对数关系--列表是线性的访问时间。

我们认为最简单的树组织形式是\ **非平衡二叉树**\ 。树内部的结点用\ ``{Key, Vlue, Smaller, Bigger}``\ 来表示。\ ``Value``\ 是被存储在树的一些结点中对象的值，它的键为\ ``Key``\ 。\ ``Smaller``\ 是一棵子树，它的所有结点的键值都小于\ ``Key``\ ，\ ``Bigger``\ 也是一棵子树，它的所有结点的键值都大于或等于\ ``Key``\ 。树的叶子用原子式\ ``nil``\ 表示。

我们从\ ``lookup(Key, Tree)``\ 函数开始，这个函数搜索\ ``Tree``\ 以确定树中是否有与\ ``Key``\ 相关的项。

.. code-block:: erlang

    lookup(Key, nil) ->
        not_found;

    lookup(Key, {Key,Value,_,_}) ->
        {found,Value};

    lookup(Key, {Key1,_,Smaller,_}) when Key < Key1 ->
        lookup(Key, Smaller);

    lookup(Key, {Key1,_,_,Bigger}) when Key > Key1 ->
        lookup(Key, Bigger).

函数\ ``insert(Key, Value, OldTree)``\ 将数据\ ``Key-Value``\ 添加到树\ ``OldTree``\ 中，并返回一棵新树。

.. code-block:: erlang

    insert(Key, Value, nil) ->
        {Key,Value,nil,nil};

    insert(Key, Value, {Key,_,Smaller,Bigger}) ->
        {Key,Value,Smaller,Bigger};

    insert(Key, Value, {Key1,V,Smaller,Bigger}) when Key < Key1 ->
        {Key1,V,insert(Key, Value, Smaller),Bigger};

    insert(Key, Value, {Key1,V,Smaller,Bigger}) when Key > Key1 ->
        {Key1,V,Smaller,insert(Key, Value, Bigger)}.

第一个子句得到数据，并插入到一棵新树当中，第二个子句将复写已经存在的结点，第三个和第四个子句确定当\ ``Key``\ 的值小于、大于或等于树中当前结点的\ ``Key``\ 时，应该采取什么样的行为。

当构建了一棵树之后，我们会想用一种方法将这棵树的结构打印出来。

.. code-block:: erlang

    write_tree(T) ->
        write_tree(0, T).

    write_tree(D, nil) ->
        io:tab(D),
        io:format('nil', []);
    write_tree(D, {Key,Value,Smaller,Bigger}) ->
        D1 = D + 4,
        write_tree(D1, Bigger),
        io:format('~n', []),
        io:tab(D),
        io:format('~w ===> ~w~n', [Key,Value]),
        write_tree(D1, Smaller).

我们可以用一个测试函数将数据插入到树中，并把它打印出来：

.. code-block:: erlang

    test1() ->
        S1 = nil,
        S2 = insert(4,joe,S1),
        S3 = insert(12,fred,S2),
        S4 = insert(3,jane,S3),
        S5 = insert(7,kalle,S4),
        S6 = insert(6,thomas,S5),
        S7 = insert(5,rickard,S6),
        S8 = insert(9,susan,S7),
        S9 = insert(2,tobbe,S8),
        S10 = insert(8,dan,S9),
        write_tree(S10).

.. topic:: 图4.1 一棵非平衡二叉树

    ::

                nil
            12 ===> fred
                        nil
                    9 ===> susan
                            nil
                        8 ===> dan
                            nil
                7 ===> kalle
                        nil
                    6 ===> thomas
                            nil
                        5 ===> rickard
                            nil
        4 ===> joe
                nil
            3 ===> jane
                    nil
                2 ===> tobbe
                    nil

注意这棵树并不是十分“平衡”。按照严格的顺序插入键的队列，比如像这样：

.. code-block:: erlang

    T1 = nil,
    T2 = insert(1,a,T1),
    T3 = insert(2,a,T2),
    T4 = insert(3,a,T3),
    T5 = insert(4,a,T4),
    ...
    T9 = insert(8,a,T8).

使这棵树看起来变成了一个列表（见图4.2）。

当键的顺序随机的时候，我们使用的方法是很好的。如果在一个插入序列里，键是有序排列的，这棵树就变成了一个列表。我们将在第??章讲述怎样构建平衡二叉树。

.. topic:: 图4.2 变化后的非平衡二叉树

    ::

                                            nil
                                    8 ===> a
                                        nil
                                7 ===> a
                                    nil
                            6 ===> a
                                nil
                        5 ===> a
                            nil
                    4 ===> a
                        nil
                3 ===> a
                    nil
            2 ===> a
                nil
        1 ===> a
            nil

我们也需要能够删除二叉树内的元素：

.. code-block:: erlang

    delete(Key, nil) ->
        nil;

    delete(Key, {Key,_,nil,nil}) ->
        nil;

    delete(Key, {Key,_,Smaller,nil}) ->
        Smaller;

    delete(Key, {Key,_,nil,Bigger}) ->
        Bigger;

    delete(Key, {Key1,_,Smaller,Bigger}) when Key == Key1 ->
        {K2,V2,Smaller2} = deletesp(Smaller),
        {K2,V2,Smaller2,Bigger};

    delete(Key, {Key1,V,Smaller,Bigger}) when Key < Key1 ->
        {Key1,V,delete(Key, Smaller),Bigger};

    delete(Key, {Key1,V,Smaller,Bigger}) when Key > Key1 ->
        {Key1,V,Smaller,delete(Key, Bigger)}.

当要删除的结点是树中的叶子，或者在这个结点下面只有一颗子树时，删除操作是很容易的（子句1到4）。子句6和7中，要删除的结点并没有被确定位置，而是继续在合适的子树中向前搜索。

在子句5当中，要删除的结点被找到，但是它是树中的一个\ ``内部``\ 结点（例如结点同时有\ ``Smaller``\ 和\ ``Bigger``\ 子树）。这种情况下,\ ``Smaller``\ 子树中具有\ ``最大``\ 键的结点将被删除，并且整棵树在这个点重建。

.. code-block:: erlang

    deletesp({Key,Value,nil,nil}) ->
        {Key,Value,nil};

    deletesp({Key,Value,Smaller,nil}) ->
        {Key,Value,Smaller};

    deletesp({Key,Value,Smaller,Bigger}) ->
        {K2,V2,Bigger2} = deletesp(Bigger),
        {K2,V2,{Key,Value,Smaller,Bigger2}}.

平衡二叉树
==========

在前面几节里，我们学会了怎样构建一棵非平衡二叉树。但不幸的是非平衡二叉树可能会变成一个列表，这样对树的插入和删除操作就是非随机的了。

一个更好的方法是保持树在任何情况下都是\ **平衡**\ 的。

Adelsom-Velskii和Landis [?]（在[?]中描述）使用一个简单的标准来衡量\ **平衡**\ 这个概念：如果一棵树的每个结点的两个子树高度之差不超过1，我们就说这棵树是平衡的。具有这种特性的树常常被称作\ *AVL*\ 树。平衡二叉树能够在\ ``O(logN)``\ 的时间规模里完成查找、插入和删除操作，\ ``N``\ 是树中结点的个数。

假设我们用元组\ ``{Key, Value, Height, Smaller, Bigger}``\ 表示一棵 AVL树，用\ ``{_, _, 0, _, _}``\ 表示一棵空树。然后在树中的查找操作就很容易实现了：

.. code-block:: erlang

    lookup(Key, {nil,nil,0,nil,nil}) ->
        not_found;

    lookup(Key, {Key,Value,_,_,_}) ->
        {found,Value};

    lookup(Key, {Key1,_,_,Smaller,Bigger}) when Key < Key1 ->
        lookup(Key,Smaller);

    lookup(Key, {Key1,_,_,Smaller,Bigger}) when Key > Key1 ->
        lookup(Key,Bigger).

``lookup``\ 的代码和非平衡二叉树中的基本一样。插入操作这样实现：

.. code-block:: erlang

    insert(Key, Value, {nil,nil,0,nil,nil}) ->
        E = empty_tree(),
        {Key,Value,1,E,E};

    insert(Key, Value, {K2,V2,H2,S2,B2}) when Key == K2 ->
        {Key,Value,H2,S2,B2};

    insert(Key, Value, {K2,V2,_,S2,B2}) when Key < K2 ->
        {K4,V4,_,S4,B4} = insert(Key, Value, S2),
    combine(S4, K4, V4, B4, K2, V2, B2);

    insert(Key, Value, {K2,V2,_,S2,B2}) when Key > K2 ->
        {K4,V4,_,S4,B4} = insert(Key, Value, B2),
        combine(S2, K2, V2, S4, K4, V4, B4).

    empty_tree() ->
        {nil,nil,0,nil,nil}.

思路是找到要插入的项将被插入到什么地方，如果插入使得树变得不平衡了，那么就平衡它。平衡一棵树的操作通过\ ``combine``\ 函数实现\ [#]_\ 。

.. code-block:: erlang

    combine({K1,V1,H1,S1,B1},AK,AV,
            {K2,V2,H2,S2,B2},BK,BV,
            {K3,V3,H3,S3,B3} ) when H2 > H1, H2 > H3 ->
                {K2,V2,H1 + 2,
                 {AK,AV,H1 + 1,{K1,V1,H1,S1,B1},S2},
                 {BK,BV,H3 + 1,B2,{K3,V3,H3,S3,B3}}
                };
    combine({K1,V1,H1,S1,B1},AK,AV,
            {K2,V2,H2,S2,B2},BK,BV,
            {K3,V3,H3,S3,B3} ) when H1 >= H2, H1 >= H3 ->
                HB = max_add_1(H2,H3),
        HA = max_add_1(H1,HB),
        {AK,AV,HA,
                {K1,V1,H1,S1,B1},
                {BK,BV,HB,{K2,V2,H2,S2,B2},{K3,V3,H3,S3,B3}}
               };
    combine({K1,V1,H1,S1,B1},AK,AV,
            {K2,V2,H2,S2,B2},BK,BV,
            {K3,V3,H3,S3,B3} ) when H3 >= H1, H3 >= H2 ->
                HA = max_add_1(H1,H2),
        HB = max_add_1(HA,H3),
        {BK,BV,HB ,
                {AK,AV,HA,{K1,V1,H1,S1,B1},{K2,V2,H2,S2,B2}},
                {K3,V3,H3,S3,B3}
               }.

    max_add_1(X,Y) when X =< Y ->
        Y + 1;
    max_add_1(X,Y) when X > Y ->
        X + 1.

打印一棵树也很简单:

.. code-block:: erlang

    write_tree(T) ->
        write_tree(0, T).

    write_tree(D, {nil,nil,0,nil,nil}) ->
        io:tab(D),
        io:format('nil', []);

    write_tree(D, {Key,Value,_,Smaller,Bigger}) ->
        D1 = D + 4,
        write_tree(D1, Bigger),
        io:format('~n', []),
        io:tab(D),
        io:format('~w ===> ~w~n', [Key,Value]),
        write_tree(D1, Smaller).

现在让我们来看看我们的劳动成果。假设我们在一棵AVL树中插入键为\ ``1,2,3,...,16``\ 的16个数据。结果如图4.3，它是一棵平衡的树了（跟上一节那棵变形的树比较一下）。

最后是AVL树中的删除操作：

.. code-block:: erlang

    delete(Key, {nil,nil,0,nil,nil}) ->
        {nil,nil,0,nil,nil};

    delete(Key, {Key,_,1,{nil,nil,0,nil,nil},{nil,nil,0,nil,nil}}) ->
        {nil,nil,0,nil,nil};

    delete(Key, {Key,_,_,Smaller,{nil,nil,0,nil,nil}}) ->
        Smaller;

    delete(Key, {Key,_,_,{nil,nil,0,nil,nil},Bigger}) ->
        Bigger;

    delete(Key, {Key1,_,_,Smaller,{K3,V3,_,S3,B3}}) when Key == Key1 ->
        {K2,V2,Smaller2} = deletesp(Smaller),
        combine(Smaller2, K2, V2, S3, K3, V3, B3);

    delete(Key, {K1,V1,_,Smaller,{K3,V3,_,S3,B3}}) when Key < K1 ->
        Smaller2 = delete(Key, Smaller),
        combine(Smaller2, K1, V1, S3, K3, V3, B3);

    delete(Key, {K1,V1,_,{K3,V3,_,S3,B3},Bigger}) when Key > K1 ->
        Bigger2 = delete(Key, Bigger),
        combine( S3, K3, V3, B3, K1, V1, Bigger2).

.. topic:: 图4.3 一棵平衡二叉树

    ::

                            nil
                        16 ===> a
                            nil
                    15 ===> a
                        nil
                14 ===> a
                        nil
                    13 ===> a
                        nil
            12 ===> a
                        nil
                    11 ===> a
                        nil
                10 ===> a
                        nil
                    9 ===> a
                        nil
        8 ===> a
                        nil
                    7 ===> a
                        nil
                6 ===> a
                        nil
                    5 ===> a
                        nil
            4 ===> a
                        nil
                    3 ===> a
                        nil
                2 ===> a
                        nil
                    1 ===> a
                        nil

``deletisp``\ 函数删除并返回树中最大的元素。

.. code-block:: erlang

    deletesp({Key,Value,1,{nil,nil,0,nil,nil},{nil,nil,0,nil,nil}}) ->
        {Key,Value,{nil,nil,0,nil,nil}};
    deletesp({Key,Value,_,Smaller,{nil,nil,0,nil,nil}}) ->
        {Key,Value,Smaller};
    deletesp({K1,V1,2,{nil,nil,0,nil,nil},
             {K2,V2,1,{nil,nil,0,nil,nil},{nil,nil,0,nil,nil}}}) ->
                {K2,V2,
                 {K1,V1,1,{nil,nil,0,nil,nil},{nil,nil,0,nil,nil}}
                };

    deletesp({Key,Value,_,{K3,V3,_,S3,B3},Bigger}) ->
            {K2,V2,Bigger2} = deletesp(Bigger),
            {K2,V2,combine(S3, K3, V3, B3, Key, Value, Bigger2)}.

.. rubric:: 脚注

.. [#] ``encode/2``\ 和本章其它一些例子的代码调用了\ ``io``\ 模块中的函数。这个模块是一个提供给用户进行格式化输入输出的标准模块。它的详细特性将在第??章和附录??中描述。
.. [#] 只有一个作者是系领带的。
.. [#] 这在数据库管理系统的\ **数据字典**\ 里面是不用怀疑的。
.. [#] 有关合并规则的详细描述可以在第[??]章找到。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
