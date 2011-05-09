.. |download-url| replace:: *http://www.ericsson.com/technology/opensource/erlang*

.. _index:

Erlang并发编程
===============

:作者: - Joe Armstrong
       - Robert Virding
       - Claes Wikstr |double-accute-o| m
       - Mike Williams
:原文: `Concurrent Programming in Erlang (PDF)`__
:译者: :ref:`志愿译者列表 <translators>`
:CPiE-CN: :ref:`《Erlang并发编程》中文版翻译计划 <cpie-cn>`
:离线浏览: 下载（\ `Tarball`__\ 、\ `PDF`__\ ）\ [*]_
:最后更新: |date|

__ http://erlang.org/download/erlang-book-part1.pdf
__ http://svn.liancheng.info/cpie-cn/trunk/.build/cpie-cn.tar.gz
__ http://svn.liancheng.info/cpie-cn/trunk/.build/cpie-cn_r148.pdf

.. important:: 作品许可协议

    《Erlang并发编程》中文版译稿采用\ `知识共享“署名、非商业性使用、禁止演绎”2.5中国大陆许可协议`__\ （\ `全文`__\ ）进行许可。

    对本书中文译稿的勘误或其他意见及建议，敬请联系\ `连城 <rhythm.mail@gmail.com>`_\ 。

    .. image:: _static/images/cc.png 

__ http://creativecommons.org/licenses/by-nc-nd/2.5/cn/
__ http://creativecommons.org/licenses/by-nc-nd/2.5/cn/legalcode

.. note:: 告读者

    这份手稿包含了\ *Concurrent Programming in Erlang*\ （ISBN 0-13-508301-X）第一部分的完整内容。

    Prentice Hall出版社允许我们将之公开。请注意，由于第二部分（应用）文本的缺失，所有对位于第二部分的章节的引用都是无效的并使用问号??代替。

    免费版本的Erlang可以从这里获得：

    |download-url|__

__ http://www.ericsson.com/technology/opensource/erlang

.. raw:: html

    <center>
      <i>
        Erricson<br>
        Telecommunications Systems Laboratories<br>
        Box 1505<br>
        S-125 25 &#x00c4;lvsj&#x0151;<br>
        Sweden<br>
      </i>
    </center>

目录：

.. toctree::
   :maxdepth: 2

   cpie-cn-project.rst
   preface.rst
   acknowledgements.rst
   introduction.rst
   part-i_index.rst

.. csv-table:: 术语字典
    :header: 原文, 翻译

    atom,                       原子式
    built-in function,          内置函数
    "concurrent, concurrency",  并发
    guard,                      保护式
    "guarded clause",           保护子句
    "parallel, parallelism",    并行
    process,                    进程
    term,                       项式
    "unguarded clause",         无保护子句
    evaluate/evaluation,        求值或执行\ [*]_
    last call optimization,     末尾调用优化

.. rubric:: 脚注

.. [*] Tarball内容为SVN最新版本，PDF内容基于SVN revision 148。感谢\ `叶玎玎 <http://twitter.com/yedingding>`__\ 友情制作PDF。
.. [*] 若上下文强调函数/BIF的返回值，则往往采用“求值”；否则采用“执行”。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
