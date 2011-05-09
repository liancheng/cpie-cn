.. |download-url| replace:: *http://www.ericsson.com/technology/opensource/erlang*

.. |liancheng-mail| image:: _static/images/liancheng-mail.png
.. |fei-mail|       image:: _static/images/fei-mail.png
.. |ken-mail|       image:: _static/images/ken-mail.png
.. |pluskid-mail|   image:: _static/images/pluskid-mail.png
.. |quark-mail|     image:: _static/images/quark-mail.png
.. |zzuduoduo-mail| image:: _static/images/zzuduoduo-mail.png
.. |weiguo-mail|    image:: _static/images/weiguo-mail.png
.. |mazha-mail|     image:: _static/images/mazha-mail.png

.. _cpie-cn:

《Erlang并发编程》翻译计划
==========================

:发起人: 连城
:译稿: :ref:`在线浏览 <index>`
:原文下载: `Concurrent Programming in Erlang (PDF)`__

__ http://erlang.org/download/erlang-book-part1.pdf

.. note::

    目前志愿者名额已满，各章节已分配完毕。除非现有志愿者出于某种原因导致无法完成所分配章节的翻译工作，否则不再征集新的志愿译者。不过仍然\ **征集志愿者进行校订工作**\ 。

《Erlang并发编程》中文版（CPiE-CN）的翻译计划完全是因个人兴趣而开始，并公开征集志愿者进行翻译。

译稿采用\ Sphinx__\ 作为文档编辑系统，并使用\ Subversion__\ 进行版本控制，Sphinx源码编码为UTF-8。文档发布服务器由连城提供。

__ http://sphinx.pocoo.org
__ http://subversion.tigris.org

.. _translators:

志愿译者
--------

以下是CPiE-CN的志愿者列表，按参与时间排序：

.. csv-table::
    :header: 姓名 / ID, Email, 主页
    :widths: 20,   30,    50

    连城,     |liancheng-mail|, http://blog.liancheng.info
    王飞,     |fei-mail|,       N/A
    Ken Zhao, |ken-mail|,       N/A
    张驰原,   |pluskid-mail|,   http://blog.pluskid.org
    丁豪,     |zzuduoduo-mail|, N/A
    赵卫国,   |weiguo-mail|,    N/A
    吴峻,     |quark-mail|,     http://lihdd.net

.. _progress:

翻译进度
--------

.. csv-table::
    :header: 章节, 译者, 状态
    :widths: 60, 20, 20

    Preface,                            连城,       完成
    Acknowledgments,                    连城,       完成
    Introduction,                       连城,       完成
    1 An ERLANG Tutorial,               连城,       完成
    2 Sequential Programming,           连城,       完成
    3 Programming with Lists,           连城,       完成
    4 Programming with Tuples,          王飞,       完成
    5 Concurrent Programming,           张驰原,     完成
    6 Distributed Programming,          Ken Zhao,   完成
    7 Error Handling,                   丁豪,       完成
    8 Programming Robust Applications,  王飞,       待校订
    9 Miscellaneous Items,              连城,       待校订
    A ERLANG Reference Grammar,         吴峻,       待校订
    B Built-in Functions,               吴峻,       进行中
    C The Standard Libraries,           赵卫国,     待校订
    D Errors in ERLANG,                 赵卫国,     待校订
    E Drivers,                          吴峻,       未开始

译者须知
--------

欢迎各位对自己的英语水平有信心且有比较充足的业余时间的同学参与翻译进程。针对本书的翻译计划，完全是出于个人兴趣，因此没有任何来自出版社或别的什么地方的进度压力，乐于参与翻译的同学不必在进度方面有所顾虑，还是以翻译质量为上，对读者负责。有兴趣的同学可以联系我（连城），Email参见\ :ref:`这里 <translators>`\ 。

凡报名参与翻译计划者，请提供以下信息：

- 姓名或ID

  总得知道怎么称呼吧 :-) 报名之后，你的大名将会出现在\ :ref:`志愿译者列表 <translators>`\ 上。

- Email

  用于平时联络。同时，你的Email也会出现在\ :ref:`志愿译者列表 <translators>`\ 上。为了防止你的邮箱被Spam，Email地址将会以图片形式出现在页面中。你可以自己制作图片，或者，如果你的Email服务商是Gmail、Hotmail、Sina、Yahoo等，可以在\ `这里`__\ 生成不错的邮箱地址图片 :-)

__ http://services.nexodyne.com/email/

- Gtalk

  可选，用于平时联络。不过工作日时最好还是邮件联系 :-)

- 个人主页地址

  可选，如果提供，也会出现在\ :ref:`志愿译者列表 <translators>`\ ，以便于让大家更好地认识你 :-)

- SVN用户名、密码

  用于给各位开通SVN提交权限。

- 目标章节

  在\ :ref:`翻译进度 <progress>`\ 中的空缺章节中选择一个或多个。

对于报名的译者，我会在开通SVN权限后发出邮件通知。

必备工具
--------

没有金刚钻，不揽瓷器活。趁手的兵器往往让你事半功倍。为了更好地参与翻译计划，你最好能够熟练使用Sphinx和Subversion（SVN）。如果二者都不熟悉，那么也可以将无格式的译文文稿通过邮件发送给我。

Subversion

    用于进行版本控制。如果你是Linux用户，那么命令行版本就挺好。如果你是Windows用户，那么推荐使用TortoiseSVN。

    在使用SVN进行提交时，为了能够让生成的HTML文档直接显示，请注意设置文件的SVN属性，主要是\ ``svn:mime-type``\ 和\ ``svn:eol-style``\ 。对于Linux用户，可以在checkout出的SVN工作目录的根目录中执行以下命令：

    .. code-block:: bash

        find . -name "*.html" | xargs svn ps 'svn:mime-type' 'text/html'
        find . -name "*.css" | xargssvn ps 'svn:mime-type' 'text/css'
        find . -name "*.js" | xargs svn ps 'svn:mime-type' 'application/x-javascript'
        find . -name "*.png" | xargs svn ps 'svn:mime-type' 'image/png'
        find . -name "*.jpg" | xargs svn ps 'svn:mime-type' 'image/jpeg'
        find . -name "*.gif" | xargs svn ps 'svn:mime-type' 'image/gif'

        find . -name "*.rst" | xargs svn ps 'svn:eol-style' 'LF'
        find . -name "*.txt" | xargs svn ps 'svn:eol-style' 'LF'

    对于Windows用户，可以使用TortoiseSVN为新加入SVN的文件单独设置SVN属性。

    方便起见，我在Sphinx自动生成的Makefile中增加了一个伪目标\ ``svn-ps``\ ，用于自动完成属性设置工作。

Sphinx

    Sphinx是一套基于reStructuredText格式的文档编辑系统，相较于LaTeX、DocBook等重量级工具而言非常地轻便好用，对于需要经常撰写技术文档的懒惰程序员而言，是不可多得的利器。

    初学者请参考\ `Sphinx 的官方文档`__\ 和\ `这篇教程`__\ 。

__ http://sphinx.pocoo.org/contents.html
__ http://scienceoss.com/use-sphinx-for-documentation

关于\ ``drafts``\ 目录
----------------------

如果你不熟悉Sphinx，而且也没有什么精力去学（真可惜），那么也可以将无格式的译文草稿提交到SVN的\ ``drafts``\ 目录，我会将之适配到Sphinx下。译文草稿的命名规范为\ ``章节名称_译者ID``\ ，如\ ``chapter-8_liancheng``\ 或\ ``appendix-a_somebody``\ 。

Sphinx译稿格式约定
------------------

所有源文件应符合以下约定：

#. 文件名采用英文，章节译稿命名规则为：

   .. parsed-literal::

       chapter-\ *n*\ .rst

   附录译稿的命名规则为：

   .. parsed-literal::

       appendix-\ *x*\ .rst

#. 使用UNIX换行符格式。
#. 字符编码统一为UTF-8 。
#. 对中英文斜体的处理：

   - 原文中对应为\ *斜体*\ 的中文文本，在译稿中请使用\ **粗体**\ 。原因正如你所见，为手写体原本就右倾的英文设计的斜体并不适合于方正的汉字。为了去除不必要的空格，请使用“\ ``\``\ ”对加粗文本前后的空格进行转义：
     ::

         这是一个\ **粗体**\ 词汇

     其效果为：

     .. parsed-literal::

         这是一个\ **粗体**\ 词汇

   - 原文中对应为斜体的英文文本，保留斜体格式。

#. 插图图片约定：

   - 图片格式请保存为PNG格式。
   - 为了保证图片比例，对原文PDF中的插图进行截图时，请将PDF文件的缩放比例调整为100%后进行截图\ [#]_\ 。

.. rubric:: 脚注

.. [#] 谢谢张驰原（pluskid）的提醒。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
