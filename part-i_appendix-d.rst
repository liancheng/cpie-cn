.. highlight:: erlang
    :linenothreshold: 3

**********************
附录D Erlang的错误处理
**********************

:翻译: 赵卫国

本附录提供了Erlang错误处理机制的细致总结。

匹配错误
========

当我们调用一个传入错误参数的内建函数时，参数不匹配的函数时，匹配错误就会产生。

当遇到匹配错误时,系统的行为可以描述成以下几种情形:

.. parsed-literal::

    if(called a BIF with bad args)then
        Error = ``badarg``
    elseif(cannot and a matching function)then
        Error = ``badmatch``
    elseif(no matching case statement)then
        Error = ``case_clause``
        ...
    if(within the scope of a “catch”)then
        Value of “catch” = ``{'EXIT', Error}``
    else
        broadcast(Error)
        die
    endif

其中“broadcast(Error)”可以描述为：

.. parsed-literal::

    if(Process has Links)then
        send ``{'EXIT', self(), Reason}`` signals to all linked
        processes
    endif

异常抛出
========

函数 ``throw(Reason)`` 的行为可以描述如下:

.. parsed-literal::

    if(within the scope of a “catch”)then
        Value of “catch” = Reason
    else
        broadcast(``nocatch``)
        die
    endif

退出信号
========

当接收到 ``{'EXIT', Pid, ExitReason}`` 信号时， Erlang 的行为可以描述成如下代码：

.. parsed-literal::

    if(ExitReason == ``kill``)then
        broadcast(``killed``) % note we change ExitReason
        die
    else
        if(trapping exits)then
            add ``{'EXIT', Pid, ExitReason}``
            to input mailbox
        else
            if(ExitReason == ``normal``) then
                continue
            else
                broadcast(ExitReason)
                die
            endif
        endif
    endif

如果进程表示符为 ``Sender`` 的进程运行一个简单的函数 ``exit(Pid,Why)`` ，那么进程 ``Pid`` 就会收到一个代表进程 ``Sender`` **好像**\ 死亡的消息 ``{'EXIT', Source, Why}`` 。

如果进程正常终止，把信号 ``{'EXIT', Source, normal}`` 发送到所有的链接进程.

函数 ``exit(Pid, kill)`` 产生一个无法销毁的消息，它使的接收进程无条件死亡，把退出的原因改为 ``killed`` 并把退出的原因发送给所有的链接进程（如若不然,可能使服务器意想不到的崩溃）.

未定义函数
==========

当涉及到未定义函数或注册进程，错误的最后一级就会发生。

如果在调用函数 ``Mod:Func(Arg0,...,ArgN)`` 但代码中没有这个函数时，就会触发 ``error_handler:undefined_function(Mod, Func, [Arg0,...,ArgN])`` 。

error_logger
============

Erlang运行时系统生成的错误消息都转化为下面这种形式:

.. code-block:: erlang

    {emulator,GroupLeader,Chars}

并把它发送给一个名为 ``error_logger`` 下的注册进程。由于所有用户代码都可以在 ``error_logger`` 中运行,因此可以很容易的把错误信息发送到其他结点上处理。这儿的变量 ``GroupLeader`` 是错误产生进程的进程表示符。有了它， ``error_logger`` 就可以把错误返回给这个产生错误的进程，以便让连接这个结点的终端打印出错误信息。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
