.. highlight:: erlang
    :linenothreshold: 3

************
附录C 标准库
************

:翻译: 赵卫国

附录C 描述了 Erlang 标准库模块的一些函数。

io
==

Io 模块提供了基本的输入输出。这儿的所有函数都有可选参数\ ``Dev``\ 它是一个用于输入输出的文件描述符。默认值是标准输入输出。

.. csv-table::
    :widths: 30, 70

    "``format([Dev],F,Args)``",  "按格式\ ``F``\ 输出参数\ ``Args``\ 。"
    "``get_chars([Dev],P,N)``",  "输出提示\ ``P``\ 并读出\ ``Dev``\ 的前\ ``N``\ 个字符。"
    "``get_line([Dev], P)``",    "输出提示\ ``P``\ 并读出\ ``Dev``\ 的一行。"
    "``n1([Dev])``",             "输出新的一行。"
    "``parse_exprs([Dev], P)``", "输出提示\ ``P``\ 并从\ ``Dev``\ 中出Erlang表达式。如果成功返回\ ``{form, ExprList}``\ , 否则返回\ ``{error, What}``\ 。"
    "``parse_form([Dev], P)``",  "输出提示\ ``P``\ ，并把\ ``Dev``\ 解析成一个\ ``Erlang`` form。如果成功返回\ ``{form, Form}``\ ，否则返回\ ``{error, What}``\ 。"
    "``put_chars([Dev], L)``",   "输出列表\ ``L``\ 中的字符。"
    "``read([Dev], P)``",        "输出提示\ ``P``\ 并且从\ ``Dev``\ 中读一项式。如果成功则返回\ ``{term,T}``\ 否则返回\ ``{error,What}``\ 。"
    "``write([Dev],Term)``",     "输出\ ``Term``\ 。"

file
====

``file``\ 模块提供了与文件系统的标准接口。

.. csv-table::
    :widths: 30, 70

    "``read file(File)``",          "返回\ ``{ok,Bin}``\ ，其中\ ``Bin``\ 是一个包含文件\ ``File``\ 内容的二进制数据对象。"
    "``write file(File, Binary)``", "把二进制数据对象\ ``Binary``\ 中的内容写入到文件\ ``File``\ 中。"
    "``get_cwd()``",                "返回\ ``{ok,Dir}``\ ，其中\ ``Dir``\ 是当前工作目录。"
    "``set cwd(Dir)``",             "把当前工作目录设为\ ``Dir``\ 。"
    "``rename(From, To)``",         "把文件名\ ``From``\ 改为\ ``To``\ 。"
    "``make dir(Dir)``",            "创建目录\ ``Dir``\ 。"
    "``del dir(Dir)``",             "删除目录\ ``Dir``\ 。"
    "``list dir(Dir)``",            "返回\ ``{ok,L}``\ ，其中\ ``L``\ 是目录\ ``Dir``\ 中的所有文件列表。"
    "``file info(File)``",          "返回\ ``{ok,L}``\ ，其中\ ``L``\ 是包含文件\ ``File``\ 信息的元组。"
    "``consult(File)``",            "如果正确返回\ ``{ok,L}``\ ，这里的\ ``L``\ 是文件\ ``File``\ 。"
    "``open(File, Mode)``",         "打开文件\ ``File``\ 的模式\ ``Mode``\ 有三种，分别是\ ``read``\ 、\ ``write``\ 和\ ``read_write``\ 。如果成功打开返回\ ``{ok,File}``\ ， 失败则返回\ ``{error,What}``\ 。"
    "``close(Desc)``",              "关闭文件\ ``Desc``\ 。"
    "``position(Desc, N)``",        "把文件\ ``Desc``\ 的当前位置设为\ ``N``\ 。"
    "``truncate(Desc)``",           "把文件\ ``Desc``\ 在当前位置截断。"

lists
=====

``list``\ 模块提供了标准列表进程函数.下面的参数中以\ ``L``\ 开头的都代表是列表。

.. csv-table::
    :widths: 60, 100

    "``append(L1, L2)``",           "返回\ ``L1+L2``\ 。"
    "``append(L)``",                "把\ ``L``\ 中所有子列表附加起来的。"
    "``concat(L)``",                "把列表\ ``L``\ 中的所有原子式合并形成一个新的原子。"
    "``delete(X, L)``",             "返回把\ ``L``\ 中第一个出现的\ ``X``\ 删除后的列表。"
    "``flat_length(L)``",           "和\ ``length(flatten(L))``\ 等价。"
    "``flatten(L)``",               "返回对L进行扁平化处理后的列表。"
    "``keydelete(Key, N, LTup)``",  "返回列表\ ``LTup``\ 删除它的第一个元组中第\ ``N``\ 个元素是\ ``Key``\ 的元组后的列表。"
    "``keysearch(Key, N, LTup)``",  "遍历元组列表\ ``LTup``\ ,查找一个第\ ``N``\ 个元素是\ ``Key``\ 的元组,若找到返回\ ``{value, X}``\ ;否则返回\ ``false``\ 。"
    "``keysort(N, LTup)``",         "返回有\ ``LTup``\ 中一系列元组的分类的版本,这其中的第\ ``N``\ 个元素用来作关键字。"
    "``member(X, L)``",             "若\ ``X``\ 是列表\ ``L``\ 中的成员返回\ ``true``, 否则返回\ ``false``\ 。"
    "``last(L)``",                  "返回\ ``L``\ 的最后一个元素。"
    "``nth(N, L)``",                "返回\ ``L``\ 的第\ ``N``\ 个元素。"
    "``reverse(L)``",               "把\ ``L``\ 中最上层的元素反转。"
    "``reverse(L1, L2)``",          "和\ ``append(reverse(L1), L2)``\ 等价。"
    "``sort(L)``",                  "对\ ``L``\ 进行排序。"

code
====

``code``\ 模块用于载入或操纵编译过的代码。

.. csv-table::
    :widths: 30, 70

     "``set_path(D)``",          "把代码服务器查询的路径设为目录\ ``D``\ 。"
     "``load_file(File)``",      "在当前路径上加载文件\ ``File.erl``\ 。加载成功返回\ ``{module, ModuleName }``\ ；失败返回\ ``{error, What}``\ 。"
     "``is_loaded(Module)``",    "检验模块\ ``Module``\ 是否已经加载.若已加载返回\ ``{file, AbsFileName}``\ ，否则返回\ ``false``\ 。"
     "``esure_loaded(Module)``", "加载之前未加载的模块,它的返回值和\ ``load_file(File)``\ 一样。"
     "``purge(Module)``",        "清楚模块\ ``Module``\ 中的代码。"
     "``all_loaded()``",         "返回所有载入模块的元组\ ``{Module, AbsFileName}``\ 。"

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
