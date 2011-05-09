.. highlight:: erlang
   :linenothreshold: 3

****************
第6章 分布式编程
****************

:翻译: Ken Zhao
:校订: 连城

本章描述如何编写运行于Erlang\ **节点**\ 网络上的分布式Erlang程序。我们描述了用于实现分布式系统的语言原语。Erlang进程可以自然地映射到分布式系统之中；同时，之前章节所介绍的Erlang并发原语和错误检测原语在分布式系统和单节点系统中仍保持原有属性。

动机
====

我们有很多理由去编写分布式应用，比如：

**速度**

    我们可以把我们的程序切分成能够分别运行于多个不同节点的几个部分。比如，某个编译器可以将一个模块里的各个函数分发到不同节点分别编译，编译器本身则负责协调各节点的活动。

    在例如一个具备一个节点池的实时系统，作业以round-robin的方式指派给不同的节点，以此降低系统的响应延迟。

**可靠性和容错**

    为了增加系统的可靠性，我们可以部署多个互相协作的节点，以求一个或多个节点的失败不致影响整个系统的运作。

**访问其他节点上的资源**

    某些软硬件资源可能只可被特定的计算机访问。

**秉承应用固有的分布式特质**

    会议系统、订票系统以及许多多计算机实时系统都属于这类应用。

**可扩展性**

    系统可以被设计成能够通过添加额外节点来增加系统的容量的形式。如果系统太慢，购买更多的处理器便可提高性能。

分布式机制
==========

以下的BIF可用于分布式编程：

``spawn(Node, Mod, Func, Args)``

    在远程节点产生一个新的进程。

``spawn_link(Node, Mod, Func, Args)``

    在远程节点产生一个新的进程并创建一个指向这个进程的链接。

``monitor_node(Node, Flag)``

    若\ ``Flag``\ 为\ ``true``\ ，该BIF令当前进程监视节点\ ``Node``\ 。如果\ ``Node``\ 出错或消失，一个\ ``{nodedown, Node}``\ 消息将被发送给当前进程，若\ ``Flag``\ 为\ ``false``\ ，则关闭监视。

``node()``

    返回当前节点名称。

``nodes()``

    返回已知的所有其他节点的名称列表。

``node(Item)``

    返回\ ``Item``\ 所处节点的名称。\ ``Item``\ 可以是Pid、引用或端口。

``disconnect_node(Nodename)``

    断开与节点\ ``Nodename``\ 的连接。

\ **节点**\ 是分布式Erlang的一个核心概念。在分布式Erlang系统中，术语\ **节点**\ 指一个可参与分布式Erlang事务的运行着的Erlang系统。独立的Erlang可通过启动一个称为网络内核的特殊进程来加入一个分布式Erlang系统。这个进程将计算BIF\ ``alive/2``\ 。网络内核将在??详述。一旦启动了网络内核，系统就处于\ **活动**\ 状态。

处于活动状态的系统会被分配一个节点名称，该名称可以通过BIF ``node(Item)``\ 获得。该名称是一个全局唯一的原子式。不同的Erlang实现中节点名称的格式可能不同，但总是一个被\ ``@``\ 分为两部分的原子式。

BIF ``node(Item)``\ 返回创建\ ``Item``\ 的节点的名称，其中\ ``Item``\ 是一个Pid、端口或引用。

BIF ``nodes/0``\ 返回网络中与当前节点连接的所有其他节点的名称列表。

BIF ``monitor_node(Node, Flag)``\ 可用于监视节点。当节点\ ``Node``\ 失败或到\ ``Node``\ 的网络连接失败时，执行了\ ``monitor_node(Node, true)``\ 的进程将收到消息\ ``{nodedown, Node}``\ 。不幸的是，我们无法区分节点失败和网络失败。例如，以下代码会一直挂起到节点\ ``Node``\ 失败为止：

.. code-block:: erlang

    .....
    monitor_node(Node, true),
    receive
        {nodedown, Node} ->
             .....
    end,
             .....

如果连接不存在，且\ ``monitor_node/2``\ 被调用，系统将尝试建立连接；若连接建立失败则投递一个\ ``nodedown``\ 消息。若针对同一节点连续两次调用\ ``monitor_node/2``\ 则在节点失败时将投递\ **两条**\ ``nodedown``\ 消息。

对\ ``monitor_node(Node, false)``\ 的调用只是递减一个计数器，该计数器用于记录\ ``Node``\ 失败时需要向调用进程发送的\ ``nodedown``\ 消息的数量。之所以这么做，是因为我们往往会用一对匹配的\ ``monitor_node(Node, true)``\ 和\ ``monitor_node(Node, false)``\ 来封装远程过程调用。

BIF ``spawn/3``\ 和\ ``spawn_link/3``\ 用于在本地节点创建新进程。要在任意的节点创建进程，需要使用BIF ``spawn/4``\ ，所以：

.. code-block:: erlang

    Pid = spawn(Node, Mod, Func, Args),

将在\ ``Node``\ 产生一个进程，而\ ``spawn_link/4``\ 会在远程节点产生一个进程并建立一个与当前进程的链接。

这两个BIF各自会返回一个Pid。若节点不存在，也会返回一个Pid，当然由于没有实际的进程被执行，这个Pid没什么用处。对于\ ``spawn_link/4``\ ，在节点不存在的情况下当前进程会收到一个“\ ``EXIT``\ ”消息。

几乎所有针对本地Pid的操作同样都对远程Pid有效。消息可以被发送至远程进程，也可以在本地进程和远程进程间建立链接，就好像远程进程执行于本地节点一样。这意味着，比方说，发送给远程进程的消息总是按发送顺序传送、不会受损也不会丢失。这些都是由运行时系统来保障的。消息接收的唯一可能的错误控制，就是由程序员掌控的\ ``link``\ 机制，以及消息发送方和接收方的显式同步。

注册进程
========

BIF ``register/2``\ 用于在本地节点上为进程注册一个名称。我们可以这样向远程节点的注册进程发送消息：

.. code-block:: erlang

    {Name, Node} ! Mess.

若在节点\ ``Node``\ 上存在一个注册为名称\ ``Name``\ 的进程，则\ ``Mess``\ 将被发送到该进程。若节点或注册进程不存在，则消息被丢弃。

连接
====

Erlang节点间存在一个语言层面的连接概念。系统初被启动时，系统无法“觉察”任何其他节点，对\ ``nodes()``\ 求值将返回\ ``[]``\ 。与其他节点间的连接不是由程序员显式建立的。到远程节点\ ``N``\ 的连接是在\ ``N``\ 首次被引用时建立的。如下所示：

.. code-block:: erlang

    1> nodes().
       []
    2> P = spawn('klacke@super.eua.ericsson.se', M, F, A).
       <24.16.1>
    3> nodes().
       ['klacke@super.eua.ericsson.se']
    4> node(P).
       'klacke@super.eua.ericsson.se'

要想建立到远程节点的连接，我们只需要在任意涉及远程节点的表达式中引用到节点的名称即可。检测网络错误的唯一手段就是使用链接BIF或\ ``monitor_node/2``\ 。要断开与某节点的连接可使用BIF ``disconnect_node(Node)``\ 。

节点之间是松散耦合的。节点可以像进程一样动态地被创建或消失。耦合不那么松散的系统可以通过配置文件和配置数据来实现。在生产环境下，通常只会部署固定数目个具备固定名称的节点。

银行业务示例
============

这一节我们将展示如何结合BIF ``monitor_node/2``\ 和向远程节点的注册进程发送消息的能力。我们将实现一个非常简单的银行服务，用以处理远程站点的请求，比如ATM机上存款、取款业务。

.. topic:: 程序6.1

    .. code-block:: erlang

        -module(bank_server).
        -export([start/0, server/1]).

        start() ->
            register(bank_server, spawn(bank_server, server, [[]])).

        server(Data) ->
            receive
                {From, {deposit, Who, Amount}} ->
                    From ! {bank_server, ok},
                    server(deposit(Who, Amount, Data));
                {From, {ask, Who}} ->
                    From ! {bank_server, lookup(Who, Data)},
                    server(Data);
                {From, {withdraw, Who, Amount}} ->
                    case lookup(Who, Data) of
                        undefined ->
                            From ! {bank_server, no},
                            server(Data);
                        Balance when Balance > Amount ->
                            From ! {bank_server, ok},
                            server(deposit(Who, -Amount, Data));
                        _ ->
                            From ! {bank_server, no},
                            server(Data)
                    end
            end.

        lookup(Who, [{Who, Value}|_]) -> Value;
        lookup(Who, [_|T]) -> lookup(Who, T);
        lookup(_, _) -> undefined.

        deposit(Who, X, [{Who, Balance}|T]) ->
            [{Who, Balance+X}|T];
        deposit(Who, X, [H|T]) ->
            [H|deposit(Who, X, T)];
        deposit(Who, X, []) ->
            [{Who, X}].

程序6.1的代码运行于银行总部。而在出纳机（或分行）中执行的是程序6.2，该程序完成与总行服务器的交互。

.. topic:: 程序6.2

    .. code-block:: erlang

        -module(bank_client).
        -export([ask/1, deposit/2, withdraw/2]).

        head_office() -> 'bank@super.eua.ericsson.se'.

        ask(Who) ->              call_bank({ask, Who}).
        deposit(Who, Amount)  -> call_bank({deposit, Who, Amount}).
        withdraw(Who, Amount) -> call_bank({withdraw, Who, Amount}).
        call_bank(Msg) ->
            Headoffice = head_office(),
            monitor_node(Headoffice, true),
            {bank_server, Headoffice} ! {self(), Msg},
            receive
                 {bank_server, Reply} ->
                     monitor_node(Headoffice, false),
                     Reply;
                 {nodedown, Headoffice} ->
                     no
            end.

客户端程序定义了三个访问总行服务器的接口函数：

``ask(Who)``

    返回客户\ ``Who``\ 的余额

``deposit(Who, Amount)``

    给客户\ ``Who``\ 的帐户里面存入资金数\ ``Amount``

``withdraw(Who, Amount)``

    尝试从客户\ ``Who``\ 的帐户里面取出资金数\ ``Amount``

函数\ ``call_bank/1``\ 实现了远程过程调用。一旦总行节点停止运作，\ ``call_bank/1``\ 将会及时发现，并返回\ ``no``\ 。

总行节点的名称是硬编码在源码中的。在后续章节中我们将展示集中隐藏该信息的手段。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
