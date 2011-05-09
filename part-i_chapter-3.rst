.. highlight:: erlang
    :linenothreshold: 3

**************
第3章 列表编程
**************

:翻译: 连城

这一章研究对列表的处理。列表是用于存储可变数量的元素的结构。列表的写法以“\ ``[``\ ”开头以“\ ``]``\ ”结尾。列表的元素以逗号分隔。例如，\ ``[E1,E2,E3,...]``\ 指代包含元素\ ``E1,E2,E3,...``\ 的列表。

标记\ ``[E1,E2,E3,...,En|Variable]``\ ，其中\ ``n`` |>=| ``1``\ ，用于表示前\ ``n``\ 个元素为\ ``E1,E2,E3,...,En``\ 其余部分由\ ``Variable``\ 指代的列表。当\ ``n = 1``\ 时，列表的形式为\ ``[H|T]``\ ；这个形式的出现频率非常高，通常将\ ``H``\ 称为列表的\ **头部**\ ，而\ ``T``\ 为列表的\ **尾部**\ 。

本章我们将讨论如何处理\ **真**\ 列表；即尾部为空列表\ ``[]``\ 的列表。

应该记住在处理\ **固定数目**\ 的元素时总是应该使用元组\ ``tuple``\ 。元组所占的存储空间仅是列表的一半并且访问也更迅速。在需要处理可变数目个元素时才应该使用列表。

用于列表处理的BIF
=================

一些内置函数可用于列表与其他数据类型间的互相转换。主要的BIF包括：

``atom_to_list(A)``

    将原子式\ ``A``\ 转换为一个ASCII字符列表。

    如：\ ``atom_to_list(hello)``\ |=>|\ ``[104,101,108,108,111]``\ [#]_\ 。

``float_to_list(F)``

    将浮点数\ ``F``\ 转换为一个ASCII字符列表。

    如：\ ``float_to_list(1.5)``\ |=>|\ ``[49,46,53,48,48,...,48]``\ 。

``integer_to_list(I)``

    将整数\ ``I``\ 转换为一个ASCII字符列表。

    如：\ ``integer_to_list(1245)``\ |=>|\ ``[[49,50,52,53]``\ 。

``list_to_atom(L)``

    将ASCII字符列表\ ``L``\ 转换为一个原子式。

    如：\ ``list_to_atom([119,111,114,108,100])``\ |=>|\ ``world``\ 。

``list_to_float(L)``

    将ASCII字符列表\ ``L``\ 转换为一个浮点数。

    如：\ ``list_to_float([51,46,49,52,49,53,57])``\ |=>|\ ``3.14159``\ 。

``list_to_integer(L)``

    将ASCII字符列表\ ``L``\ 转换为一个整数。

    如：\ ``list_to_integer([49,50,51,52])``\ |=>|\ ``1234``\ 。

``hd(L)``

    返回列表\ ``L``\ 的第一个元素。

    如：\ ``hd([a,b,c,d])``\ |=>|\ ``a``\ 。

``tl(L)``

    返回列表\ ``L``\ 的尾部。

    如：\ ``tl([a,b,c,d])``\ |=>|\ ``[b,c,d]``\ 。

``length(L)``

    返回列表\ ``L``\ 的长度。

    如：\ ``length([a,b,c,d])``\ |=>|\ ``4``\ 。

有两个BIF ``tuple_to_list/1``\ 和\ ``list_to_tuple/1``\ 将放在第??章讨论。还有一些列表处理相关的BIF，如\ ``list_to_pid(AsciiList)``\ 、\ ``pid_to_list(Pid)``\ 。这些将在附录B中描述。

常用列表处理函数
================

以下各小节给出了一些简单列表处理函数的使用示例。这里所描述的所有函数都包含在标准Erlang发行版的\ ``lists``\ 模块中（细节参见附录C）。

``member``
----------

``member(X, L)``\ 在\ ``X``\ 是列表\ ``L``\ 的元素时返回\ ``true``\ ，否则返回\ ``false``\ 。

.. code-block:: erlang

    member(X, [X|_]) -> true;
    member(X, [_|T]) -> member(X, T);
    member(X, []) -> false.

``member``\ 的第一个子句匹配的是\ ``X``\ 为列表的第一个元素的情况，这种情况下\ ``member``\ 返回\ ``true``\ 。如果第一个子句不匹配，则第二个子句将匹配第二个参数是非空列表的情况，这种情况下模式\ ``[_|T]``\ 匹配一个非空列表并将\ ``T``\ 绑定到列表的尾部，然后以原来的\ ``X``\ 及列表的尾部\ ``T``\ 递归调用\ ``member``\ 。\ ``member``\ 前两个子句就是在说当\ ``X``\ 是列表的\ **第一个**\ 元素（头部），或它被包含在列表的剩余部分（尾部）中时，\ ``X``\ 就是该列表的一个成员。\ ``member``\ 的第三个子句是说\ ``X``\ 不可能是空列表\ ``[]``\ 的成员，并因此返回\ ``false``\ 。

我们将\ ``member``\ 的求值过程列举如下：

.. code-block:: erlang

    > lists:member(a,[1,2,a,b,c]).
    (0)lists:member(a,[1,2,a,b,c])
    (1).lists:member(a, [2,a,b,c])
    (2)..lists:member(a,[a,b,c])
    (2)..true
    (1).true
    (0)true
    true
    > lists:member(a,[1,2,3,4]).
    (0)lists:member(a, [1,2,3,4])
    (1).lists:member(a, [2,3,4])
    (2)..lists:member(a, [3,4])
    (3)...lists:member(a, [4])
    (4)....lists:member(a, [])
    (4)....false
    (3)...false
    (2)..false
    (1).false
    (0)false
    false

``append``
----------

``append(A,B)``\ 连接两个列表\ ``A``\ 和\ ``B``\ 。

.. code-block:: erlang

    append([H|L1], L2) -> [H|append(L1, L2)];
    append([], L) -> L.

``append``\ 的第二个子句再明白不过了——将任意列表\ ``L``\ 追加至空列表之后仍得到\ ``L``\ 。

第一个子句给出了追加一个非空列表到另一个列表之后的规则。因此，对于：

.. code-block:: erlang

    append([a,b,c], [d,e,f])

其求值结果为：

.. code-block:: erlang

    [a | append([b,c], [d,e,f])]

那么\ ``append([b,c], [d,e,f])``\ 的值又是多少呢？它（当然）是\ ``[b,c,d,e,f]``\ ，因此\ ``[a | append([b,c], [d,e,f])]``\ 的值就是\ ``[a|append([b,c], [d,e,f])]``\ ，这也是\ ``[a,b,c,d,e,f]``\ 的另一种写法。

``append``\ 的行为如下：

.. code-block:: erlang

    > lists:append([a,b,c],[d,e,f]).
    (0)lists:append([a,b,c],[d,e,f])
    (1).lists:append([b,c], [d,e,f])
    (2)..lists:append([c],[d,e,f])
    (3)...lists:append([], [d,e,f])
    (3)...[d,e,f]
    (2)..[c,d,e,f]
    (1).[b,c,d,e,f]
    (0)[a,b,c,d,e,f]
    [a,b,c,d,e,f]

``reverse``
-----------

``reverse(L)``\ 用于颠倒列表\ ``L``\ 中的元素顺序。

.. code-block:: erlang

    reverse(L) -> reverse(L, []).

    reverse([H|T], Acc) ->
        reverse(T, [H|Acc]);
    reverse([], Acc) ->
        Acc.

``reverse(L)``\ 利用一个\ **辅助**\ 函数\ ``reverse/2``\ 将最终结果累积到第二个参数中。
 
调用\ ``reverse(L, Acc)``\ 时，若\ ``L``\ 是一个非空列表，则将\ ``L``\ 的第一个元素移除并\ **添加**\ 到\ ``Acc``\ 的头部。因此对\ ``reverse([x,y,z], Acc)``\ 的调用将导致\ ``reverse([y,z], [x|Acc])``\ 的调用。最终\ ``reverse/2``\ 的第一个参数将归结为一个空列表，这时\ ``reverse/2``\ 的第二个子句将被匹配并另函数结束。

整个过程如下：

.. code-block:: erlang

    > lists:reverse([a,b,c,d]).
    (0)lists:reverse([a,b,c,d])
    (1).lists:reverse([a,b,c,d], [])
    (2)..lists:reverse([b,c,d], [a])
    (3)...lists:reverse([c,d], [b,a])
    (4)....lists:reverse([d], [c,b,a])
    (5).....lists:reverse([], [d,c,b,a])
    (5).....[d,c,b,a]
    (4)....[d,c,b,a]
    (3)...[d,c,b,a]
    (2)..[d,c,b,a]
    (1).[d,c,b,a]
    (0)[d,c,b,a]
    [d,c,b,a]

``delete_all``
--------------

``delete_all(X, L)``\ 用于删除列表\ ``L``\ 中出现的所有\ ``X``\ 。

.. code-block:: erlang

    delete_all(X, [X|T]) ->
        delete_all(X, T);
    delete_all(X, [Y|T]) ->
        [Y | delete_all(X, T)];
    delete_all(_, []) ->
        [].

``delete_all``\ 所使用的递归模式与\ ``member``\ 和\ ``append``\ 类似。

``delete_all``\ 的第一个子句在要删除的元素出现在列表的头部时匹配。

示例
====

在以下章节中我们将给出一些稍微复杂一些的列表处理函数的使用示例。

``sort``
--------

程序3.1是著名的快速排序的一个变体。\ ``sort(X)``\ 对列表\ ``X``\ 的元素排序，将结果放入一个新列表并将之返回。

.. topic:: 程序3.1

    .. code-block:: erlang

        -module(sort).
        -export([sort/1]).

        sort([]) -> [];
        sort([Pivot|Rest]) ->
            {Smaller, Bigger} = split(Pivot, Rest),
            lists:append(sort(Smaller), [Pivot|sort(Bigger)]).

        split(Pivot, L) ->
            split(Pivot, L, [], []).

        split(Pivot, [], Smaller, Bigger) ->
            {Smaller,Bigger};
        split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
            split(Pivot, T, [H|Smaller], Bigger);
        split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
            split(Pivot, T, Smaller, [H|Bigger]).

此处选取列表的第一个元素为中轴。元列表被分为两个列表\ ``Smaller``\ 和\ ``Bigger``\ ：\ ``Smaller``\ 的所有元素都小于中轴\ ``Pivot``\ 而\ ``Bigger``\ 的所有元素都大于等于\ ``Pivot``\ 。之后，再对列表\ ``Smaller``\ 和\ ``Bigger``\ 分别排序并将结果合并。

函数\ ``split({Pivot, L})``\ 返回元组\ ``{Smaller, Bigger}``\ ，其中所有\ ``Bigger``\ 中的元素都大于等于\ ``Pivot``\ 而所有\ ``Smaller``\ 中的元素都小于\ ``Pivot``\ 。\ ``split(Pivot, L)``\ 通过调用一个辅助函数\ ``split(Pivot, L, Smaller, Bigger)``\ 完成任务。两个累加列表，\ ``Smaller``\ 和\ ``Bigger``\ 分别用于存储\ ``L``\ 中小于和大于等于\ ``Pivot``\ 的元素。\ ``split/4``\ 的代码与\ ``reverse/2``\ 非常相像，只是多用了一个累加列表。例如：

.. code-block:: erlang

    > lists:split(7,[2,1,4,23,6,8,43,9,3]).
    {[3,6,4,1,2],[9,43,8,23]}

如果我们调用\ ``sort([7,2,1,4,23,6,8,43,9,3])``\ ，首先就会以\ ``7``\ 为中轴来调用\ ``split/2``\ 。这将产生两个列表：所有元素都小于中轴\ ``7``\ 的\ ``[3,6,4,1,2]``\ ，以及所有元素都大于等于中轴的\ ``[9,43,8,23]``\ 。

假设\ ``sort``\ 工作正常，则\ ``sort([3,6,4,1,2])``\ |=>|\ ``[1,2,3,4,6]``\ 而\ ``sort([9,43,8,23])``\ |=>|\ ``[8,9,23,43]``\ 。最后，排好序的列表被拼装在一起：

.. code-block:: erlang

    > append([1,2,3,4,6], [7 | [8,9,23,43]]).
    [1,2,3,4,6,7,8,9,23,43]

再动一点脑筋，都\ ``append``\ 的调用也可以省掉，如下所示：

.. code-block:: erlang

    qsort(X) ->
        qsort(X, []).

    %% qsort(A,B)
    %%   Inputs:
    %%      A = unsorted List
    %%      B = sorted list where all elements in B
    %%          are greater than any element in A
    %%   Returns
    %%      sort(A) appended to B

    qsort([Pivot|Rest], Tail) ->
        {Smaller,Bigger} = split(Pivot, Rest),
        qsort(Smaller, [Pivot|qsort(Bigger,Tail)]);
    qsort([], Tail) ->
        Tail.

我们可以利用BIF\ ``statistics/1``\ （用于提供系统性能相关的信息，参见附录??）将之与第一版的\ ``sort``\ 做一个对比。如果我们编译并执行以下代码片段：

.. code-block:: erlang

    ...
    statistics(reductions),
    lists:sort([2,1,4,23,6,7,8,43,9,4,7]),
    {_, Reductions1} = statistics(reductions),
    lists:qsort([2,1,4,23,6,7,8,43,9,4,7]),
    {_, Reductions2} = statistics(reductions),
    ...

我们可以得知\ ``sort``\ 和\ ``qsort``\ 的归约（函数调用）次数。在我们的示例中\ ``sort``\ 花费\ ``93``\ 次归约，而\ ``qsort``\ 花费\ ``74``\ 次，提升了百分之二十。

集合
----

程序3.2是一组简单的集合操作函数。在Erlang中表示集合的最直白的方法就是采用一个不包含重复元素的无序列表。

集合操作函数如下：

``new()``

    返回一个空集合。

``add_element(X, S)``

    返回将元素\ ``X``\ 并入集合\ ``S`` 产生的新集合。

``del_element(X, S)``

    返回从集合\ ``S``\ 中删去元素\ ``X``\ 的新集合。

``is_element(X, S)``

    当元素\ ``X``\ 在集合\ ``S``\ 中时返回\ ``true``\ ，否则返回\ ``false``\ 。

``is_empty(S)``

    当集合\ ``S``\ 为空集时返回\ ``true``\ ，否则返回\ ``false``\ 。

``union(S1, S2)``

    返回集合\ ``S1``\ 和\ ``S2``\ 的并集，即包含了\ ``S1``\ 或\ ``S2``\ 所有元素的集合。

``intersection(S1, S2)``

    返回集合\ ``S1``\ 和\ ``S2``\ 的交集，即仅包含既包含于\ ``S1``\ 又包含于\ ``S2``\ 的元素的集合。

严格地说，我们并不能说\ ``new``\ 返回了一个空集，而应该说\ ``new``\ 返回了一个空集的\ **表示**\ 。如果我们将集合表示为列表，则以上的集合操作可以编写如下：

.. topic:: 程序3.2

    .. code-block:: erlang

        -module(sets).
        -export([new/0, add_element/2, del_element/2,
                 is_element/2, is_empty/1, union/2, intersection/2]).

        new() -> [].

        add_element(X, Set) ->
            case is_element(X, Set) of
                true -> Set;
                false -> [X|Set]
            end.

        del_element(X, [X|T]) -> T;
        del_element(X, [Y|T]) -> [Y|del_element(X,T)];
        del_element(_, [])    -> [].

        is_element(H, [H|_])   -> true;
        is_element(H, [_|Set]) -> is_element(H, Set);
        is_element(_, [])      -> false.

        is_empty([]) -> true;
        is_empty(_)  -> false.

        union([H|T], Set) -> union(T, add_element(H, Set));
        union([], Set)    -> Set.

        intersection(S1, S2)       -> intersection(S1, S2, []).
        intersection([], _, S)     -> S;
        intersection([H|T], S1, S) ->
            case is_element(H,S1) of
                true -> intersection(T, S1, [H|S]);
                false -> intersection(T, S1, S)
            end.

运行程序3.2的代码：

.. code-block:: erlang

    > S1 = sets:new().
    []
    > S2 = sets:add_element(a, S1).
    [a]
    > S3 = sets:add_element(b, S2).
    [b,a]
    > sets:is_element(a, S3).
    true
    > sets:is_element(1, S2).
    false
    > T1 = sets:new().
    []
    > T2 = sets:add_element(a, T1).
    [a]
    > T3 = sets:add_element(x, T2).
    [x,a]
    > sets:intersection(S3, T3).
    [a]
    10> sets:union(S3,T3).
    [b,x,a]

这个实现并不十分高效，但足够简单以保证其正确性（但愿如此）。今后还可以将之替换为一套更高效的实现。

素数
----

在我们的最后一个例子（程序3.3）中，我们将来看看如何使用\ **埃拉托色尼筛法**\ 来生成一张素数表。

.. topic:: 程序 3.3

    .. code-block:: erlang

        -module(siv).
        -compile(export_all).

        range(N, N) ->
            [N];
        range(Min, Max) ->
            [Min | range(Min+1, Max)].

        remove_multiples(N, [H|T]) when H rem N == 0 ->
            remove_multiples(N, T);
        remove_multiples(N, [H|T]) ->
            [H | remove_multiples(N, T)];
        remove_multiples(_, []) ->
            [].

        sieve([H|T]) ->
            [H | sieve(remove_multiples(H, T))];
        sieve([]) ->
            [].

        primes(Max) ->
            sieve(range(2, Max)).

注意在程序3.3中我们使用了编译器标注\ ``-compile(export_all)``\ ——这将隐式地导出该模块中的所有函数，于是我们无须显式地给出导出申明便可以调用这些函数。

``range(Min, Max)``\ 返回一个包含从\ ``Min``\ 到\ ``Max``\ 的所有整数的列表。

``remove_multiples(N, L)``\ 从列表\ ``L``\ 删除中\ ``N``\ 的倍数：

.. code-block:: erlang

    > siv:range(1,15).
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
    > siv:remove_multiples(3,[1,2,3,4,5,6,7,8,9,10]).
    [1,2,4,5,7,8,10]

``sieve(L)``\ 保留列表\ ``L``\ 的头部，对于尾部的列表，则再递归地删除其头部的所有倍数：

.. code-block:: erlang

    > siv:primes(25).
    [2,3,5,7,11,13,17,19,23]

列表的常用递归模式
==================

尽管一个典型的程序往往会使用很多不同的函数来操作列表，但大多数列表处理函数都是由少数几种模式演变而来。大部分列表处理函数无非就是在干着这些事情：

- 在一个列表中寻找一个元素，并在找到时做些事情。
- 对输入列表的每个元素做些事情并构造一个与其结构相同的输出列表。
- 在遇到列表中的第\ *n*\ 个元素时做些事情。
- 对列表进行扫描，并构造一个或一组与原列表相关的新列表。

我们将以此对其进行讨论。

搜索列表元素
------------

给定以下递归模式：

.. code-block:: erlang

    search(X, [X|T]) ->
        ... do something ...
        ...;
    search(X, [_|T]) ->
        search(X, T);
    search(X, []) ->
        ... didn't find it ...

第一种情况匹配的是找到了我们所感兴趣的项的情形。第二种情况在列表的头部不是我们所感兴趣的项时匹配，这时将接着处理列表的尾部。最后一种情况匹配的是列表元素耗尽的情形。

将以上代码与\ ``member/2``\ 的代码（第??节）作个比较，我们可以看到我们不过是把\ ``... do something ...``\ 换成了\ ``true``\ ，把\ ``... didn't find it ...``\ 换成了\ ``false``\ 。

构建同构列表
------------

有时我们会想构造一个\ **形如**\ 输入列表的列表，但同时又要对输入列表的每个元素做些操作。这时可以这么写：

.. code-block:: erlang

    isomorphic([X|T]) ->
        [something(X)|isomorphic(T)];
    isomorphic([]) ->
        [].

然后，比如我们想写一个将给定列表中的所有元素翻倍的函数，我们就可以这么写：

.. code-block:: erlang

    double([H|T]) ->
        [2 * H | double(T)];
    double([]) ->
        [].

于是便有：

.. code-block:: erlang

    > lists1:double([1,7,3,9,12]).
    [2,14,6,18,24]

事实上这种手法只能作用于列表的\ **最上层**\ ，因此如果我们想遍历列表的所有层次，我们就得将函数定义修改如下：

.. code-block:: erlang

    double([H|T]) when integer(H)->
        [2 * H | double(T)];
    double([H|T]) when list(H) ->
        [double(H) | double(T)];
    double([]) ->
        [].

后一个版本就可以成功遍历深层的嵌套列表了：

.. code-block:: erlang

    > lists1:double([1,2,[3,4],[5,[6,12],3]]).
    [2,4,[6,8],[10,[12,24],6]]

计数
----

我们常常要使用到计数器，以便对一个列表的第\ *n*\ 个元素做些动作：

.. code-block:: erlang

    count(Terminal, L) ->
        ... do something ...;
    count(N, [_|L]) ->
        count(N-1, L).

则返回列表中第\ *n*\ 个元素（假设其存在）的函数可以写成：

.. code-block:: erlang

    nth(1, [H|T]) ->
        H;
    nth(N, [_|T]) ->
        nth(N - 1, T).

这种递减至一个终止条件的计数方式往往要由于递增计数。为了说明这一点，考虑同样是返回第\ *n*\ 个元素但是采用递增计数的函数\ ``nth1``\ ：

.. code-block:: erlang

    nth1(N, L) ->
        nth1(1, N, L).
    nth1(Max, Max, [H|_]) ->
        H;
    nth1(N, Max, [_|T]) ->
        nth1(N+1, Max, T).

这种做法需要一个额外的参数和一个辅助函数。

收集列表元素
------------

现在我们希望对一个列表中的元素做些动作，生成一个或一组新的列表。对应的模式如下：

.. code-block:: erlang

    collect(L) ->
        collect(L, []).

    collect([H|T], Accumulator) ->
        case pred(H) of
            true ->
                collect(T, [dosomething(H)|Accumulator]);
            false ->
                collect(T, Accumulator)
        end;
    collect([], Accumulator) ->
        Accumulator.

在这里我们引入了一个多出一个参数的辅助函数，多出的这个参数用于存储最终要被返回给调用方的列表。

借助这样一种模式，举个例子，我们可以写这样的一个函数：计算输入列表的所有偶元素的平方并删除所有奇元素：

.. code-block:: erlang

    funny(L) ->
        funny(L, []).

    funny([H|T], Accumulator) ->
        case even(H) of
            true -> funny(T, [H*H|Accumulator]);
            false -> funny(T, Accumulator)
        end;
    funny([], Accumulator) ->
        Accumulator.

于是有：

.. code-block:: erlang

    > lists:funny([1,2,3,4,5,6])
    [36,16,4]

注意在这种情况下结果列表中的元素的顺序与原列表中对应元素的顺序是相反的。

在递归过程中使用累加列表来构造结果经常是一种推荐的做法。这样可以编写出运行时只适用常数空间的\ **扁平**\ 的代码（细节参见第??节）。

函数式参数
==========

将函数名作为参数传递给另一个函数是一种很有用的抽象特定函数行为的方法。本节将给出两个使用这种编程技术的示例。

map
---

函数\ ``map(Func, List)``\ 返回一个列表\ ``L``\ ，其中的元素由函数\ ``Func``\ 依次作用于列表\ ``List``\ 的各个元素得到。

.. code-block:: erlang

    map(Func, [H|T]) ->
        [apply(F, [H])|map(Func, T)];
    map(Func, []) ->
        [].

    > lists:map({math,factorial}, [1,2,3,4,5,6,7,8]).
    [1,2,6,24,120,720,5040,40320]

filter
------

函数\ ``filter(Pred, List)``\ 对列表\ ``List``\ 中的元素进行过滤，仅保留令\ ``Pred``\ 的值为\ ``true``\ 的元素。这里的\ ``Pred``\ 是一个返回\ ``true``\ 或\ ``false``\ 的函数。

.. code-block:: erlang

    filter(Pred, [H|T]) ->
        case apply(Pred,[H]) of
            true ->
            [H|filter(Pred, T)];
        false ->
            filter(Pred, T)
        end;
    filter(Pred, []) ->
        [].

假设函数\ ``math:even/1``\ 在参数为偶数时返回\ ``true``\ ，否则返回\ ``fale``\ ，则：

.. code-block:: erlang

    > lists:filter({math,even}, [1,2,3,4,5,6,7,8,9,10]).
    [2,4,6,8,10]

.. rubric:: 脚注

.. [#] 标记\ ``Lhs``\ |=>|\ ``Rhs``\ 代表对\ ``Lhs``\ 求值的结果为\ ``Rhs``\ 。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
