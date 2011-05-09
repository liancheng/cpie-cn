.. highlight:: erlang
    :linenothreshold: 3

************************
第8章 编写健壮的应用程序
************************

:翻译: 王飞
:校对: 连城

第7章讲解了Erlang的错误处理机制。这一章我们来看看怎样使用这些机制来构建健壮、容错的系统。

防范错误数据
============

回想一下在第??章（程序??.5）中描述的那个用来分析电话号码的服务程序。它的主循环包含了以下代码：

.. code-block:: erlang

    server(AnalTable) ->
        receive
            {From, {analyse,Seq}} ->
                Result = lookup(Seq, AnalTable),
                From ! {number_analyser, Result},
                server(AnalTable);
            {From, {add_number, Seq, Key}} ->
                From ! {number_analyser, ack},
                server(insert(Seq, Key, AnalTable))
	    end.

以上的\ ``Seq``\ 是一个表示电话号码的数字序列，如\ ``[5,2,4,8,9]``\ 。在编写\ ``lookup/2``\ 和\ ``insert/3``\ 这两个函数时，我们应检查\ ``Seq``\ 是否是一个电话拨号按键字符\ [#]_\ 的列表。若不做这个检查，假设\ ``Seq``\ 是一个原子项\ ``hello``\ ，就会导致运行时错误。一个简单些的做法是将\ ``lookup/2``\ 和\ ``insert/3``\ 放在一个\ ``catch``\ 语句的作用域中求值：

.. code-block:: erlang

    server(AnalTable) ->
        receive
	        {From, {analyse,Seq}} ->
	            case catch lookup(Seq, AnalTable) of
		            {'EXIT', _} ->
		                From ! {number_analyser, error};
		            Result ->
		                From ! {number_analyser, Result}
		        end,
		        server(AnalTable);
	        {From, {add_number, Seq, Key}} ->
	            From ! {number_analyser, ack},
		        case catch insert(Seq, Key, AnalTable) of
		            {'EXIT', _} ->
		                From ! {number_analyser, error},
			            server(AnalTable); % Table not changed
		            NewTable ->
		                server(NewTable)
		        end
	    end.

注意，借助\ ``catch``\ 我们的号码分析函数可以只处理正常情况，而让Erlang的错误处理机制去处理\ ``badmatch``\ 、\ ``badarg``\ 、\ ``function_clause``\ 等错误。

一般来说，设计服务器时应注意即使面对错误的输入数据，服务器也不会“崩溃”。很多情况下发送给服务器的数据都来自服务器的访问函数。在上面的例子中，号码分析服务器获悉的客户端进程标识\ ``From``\ 是从访问函数获得的，例如：

.. code-block:: erlang

    lookup(Seq) ->
        number_analyser ! {self(), {analyse,Seq}},
        receive
            {number_analyser, Result} ->
                Result
        end.

服务器不需要检查\ ``From``\ 是否是一个进程标识。在这个案例中，我们（借助访问函数）来防范意外的错误情况。然而恶意程序仍然可以绕过访问函数，向服务器发送恶意数据致使服务器崩溃：

.. code-block:: erlang

    number_analyser ! {55, [1,2,3]}

这样一来号码分析器将试图向进程\ ``55``\ 发送分析结果，继而崩溃。

健壮的服务进程
==============

讲解可靠服务进程设计的最好方法就是借助实例。

第??章（程序??.6）给出了一个资源分配器。对于这个分配器，如果一个资源被分配给了进程，而这个进程在释放资源之前终止（无论是出于意外还是正常终止），那么这个资源就无法被收回。这个问题可以通过以下的方法来解决：

- 令服务程序捕捉\ ``EXIT``\ 信号（\ ``process_flag(trap_exit, true)``\ ）。
- 在分配器和申请资源的进程之间建立连接。
- 处理由这些进程发出的\ ``EXIT``\ 信号。

正如图 8.1 所示。

.. figure:: _static/images/8.1.png

    图8.1 健壮的分配器进程和客户进程

分配器的访问函数不变。通过以下方式启动分配器：

.. code-block:: erlang

    start_server(Resources) ->
        process_flag(trap_exit, true),
	    server(Resources, []).

为了接收\ ``EXIT``\ 信号，我们将 “服务器” 循环改为：

.. code-block:: erlang

    server(Free, Allocated) ->
        receive
	        {From,alloc} ->
	            allocate(Free, Allocated, From);
	        {From,{free,R}} ->
	            free(Free, Allocated, From, R);
	        {'EXIT', From, _ } ->
	            check(Free, Allocated, From)
	    end.

为了跟申请资源（如果还有资源可用）的进程建立连接，还需要修改\ ``allocate/3`` 。

.. code-block:: erlang

    allocate([R|Free], Allocated, From) ->
	    link(From),
	    From ! {resource_alloc,{yes,R}},
	    server(Free, [{R,From}|Allocated]);
    allocate([], Allocated, From) ->
	    From ! {resource_alloc,no},
	    server([], Allocated).

``free/4``\ 更复杂些：

.. code-block:: erlang

    free(Free, Allocated, From, R) ->
        case lists:member({R, From}, Allocated) of
	        true ->
	            From ! {resource_alloc, yes},
		        Allocated1 = lists:delete({R, From}, Allocated),
		        case lists:keysearch(From, 2, Allocated1) of
		            false ->
		                unlink(From);
		            _ ->
		                true
		        end,
		        server([R|Free], Allocated1);
	        false ->
	            From ! {resource_alloc, error},
	            server(Free, Allocated)
	    end.

首先我们检查将要被释放的资源，的确是分配给想要释放资源的这个进程的。如果是的话，\ ``lists:member({R, From}, Allocated)``\ 返回\ ``true``\ 。我们像之前那样建立一个新的链表来存放被分配出去的资源。我们不能只是简单的\ ``unlink From``\ ，而必须首先检查\ ``Form``\ 是否持有其他资源。如果\ ``keysearch(From, 2, Allocated1)``\ （见附录??）返回了\ ``false``\ ，\ ``From``\ 就没有持有其他资源，这样我们就可以\ ``unlink From``\ 了。

如果一个我们与之建立了link关系的进程终止了，服务程序将会收到一个\ ``EXIT``\ 信号，然后我们调用\ ``Check(Free, Allocated, From)``\ 函数。

.. code-block:: erlang

     check(Free, Allocated, From) ->
         case lists:keysearch(From, 2, Allocated) of
	     false ->
	         server(Free, Allocated);
	     {value, {R, From}} ->
	         check([R|Free],
		 lists:delete({R, From}, Allocated), From)
	 end.

如果\ ``lists:keysearch(From, 2, Allocated)``\ 返回了\ ``false``\ ，我们就没有给这个进程分配过资源。如果返回了\ ``{value, {R, From}}``\ ，我们就能知道资源\ ``R``\ 被分配给了这个进程，然后我们必须在继续检查该程序是否还持有其他资源之前，将这个资源添加到未分配资源列表，并且将他从已分配资源列表里删除。注意这种情况下我们不需要手动的与该进程解除连接，因为当它终止的时候，连接就已经解除了。

释放一个没有被分配出去的资源是可能一个严重的错误。我们应当修改程序??.6中的\ ``free/1``\ 函数，以便杀死试图这样干的程序：\ [#]_\ 。

.. code-block:: erlang

     free(Resource) ->
         resource_alloc ! {self(),{free,Resource}},
	     receive
	         {resource_alloc, error} ->
		     exit(bad_allocation); % exit added here
		 {resource_alloc, Reply} ->
		     Reply
	     end.

用这种方法杀死的程序，如果它还持有其他资源，同时还与服务程序保持着连接，那么服务程序因此将收到一个\ ``EXIT``\ 信号，如上面所述，处理这个信号的结果会是资源被释放。

以上内容说明了这么几点：

- 通过设计这样一种服务程序接口，使得客户端通过访问函数（这里是\ ``allocate/0``\ 和\ ``free/1``\ ）访问服务程序，并且防止了危险的“幕后操作”。客户端和服务程序之间的连接对用户来说是透明的。特别是客户端不需要知道服务程序的进程ID，因此也就不能干涉它的运行。

- 一个服务程序如果捕获\ ``EXIT``\ 信号，并且和它的客户端建立连接以便能监视它的话，就可以在客户端进程死亡的时候采取适当的处理行为。

分离计算部分
============

在一些程序里，我们可能希望将计算部分完全隔离出来，以免影响其它程序。Erlang shell就是这样一个东西。第??章那个简单的shell是有缺陷的。在它里面运行的一个表达式可能通过这几种方式影响到进程：

- 它可以发送进程标示符给其他进程（\ ``self/0``\ ），然后就可以与这个进程建立连接，给它发送消息。
- 它可以注册或注销一个进程

程序8.1用另外一种方法实现了一个shell：

.. topic:: 程序8.1

    .. code-block:: erlang

        -module(c_shell).
        -export([start/0, eval/2]).

        start() ->
            process_flag(trap_exit, true),
            go().

        go() ->
            eval(io:parse_exprs('-> ')),
            go().

        eval({form, Exprs}) ->
            Id = spawn_link(c_shell, eval, [self(), Exprs]),
            receive
                {value, Res, _} ->
                io:format("Result: ~w~n", [Res]),
                    receive
                    {'EXIT', Id, _ } ->
                        true
                end;
            {'EXIT', Id, Reason} ->
                io:format("Error: ~w!~n", [Reason])
            end;

        eval(_) ->
            io:format("Syntax Error!~n", []).

        eval(Id, Exprs) ->
            Id ! eval:exprs(Exprs, []).

shell进程捕获\ ``EXIT``\ 信号。命令在一个与shell进程连接的单独的进程（\ ``spawn_link(c_shell, eval, [self(), Exprs])``\ ）中运行。尽管事实上我们把shell进程的进程ID给了\ ``c_shell:eval/2``\ ，但是因为对于作为实际执行者的\ ``eval:exprs/2``\ 函数，并没有给它任何参数，因此也就不会对造成影响。

保持进程存活
============

一些进程可能对系统来说是非常重要的。例如，在一个常规的分时系统里，常常每一个终端连接都由一个负责输入输出的进程来服务。如果这个进程终止了，终端也就不可用了。程序8.2通过重启终止的进程来保持进程存活。

这个注册为\ ``keep_alive``\ 的服务程序保有一个由\ ``{Id, Mod, Func, Args}``\ 模式元组构成的列表，这个列表包含了所有正在运行的进程的标识符、模块、函数和参数。 它使用BIF ``spawn_link/3``\ 启动这些进程，因此它也和每一个进程建立连接。然后这个服务程序就开始捕获\ ``EXIT``\ 信号，当一个进程终止了，它就会收到一个\ ``EXIT``\ 信号。在搜索了那个由元组构成的列表之后，它就能重启这个进程。

不过程序8.2当然也需要改进。如果从进程列表里移除一个进程是不可能的话，那么当我们试图用一个并不存在的\ ``module:function/arity``\ 来创建进程，程序就会进入死循环。建立一个没有这些缺陷的程序，就作为练习留给读者来完成。

讨论
====

当进程收到了一个“原因”不是\ ``normal``\ 的信号，默认行为是终止自己，并通知与它相连接的进程（见第??节）。通过使用连接和捕捉\ ``EXIT``\ 信号建立一个分层的系统是不难的。在这个系统最顶层的进程（应用进程）并不捕获\ ``EXIT``\ 信号。具有依赖关系的进程相互连接。底层进程（操作系统进程）捕获\ ``EXIT``\ 并且和需要监视的应用进程（见图8.2）建立连接。使用这种操作系统结构的例子是交换机服务器和电话应用程序，将在第??章讲述，第??章是它们的文件系统。

一个因为\ ``EXIT``\ 信号导致异常的应用进程，将会把信号发送给所有跟它处在通一进程集内的进程，因此整个进程集都会被杀死。连接到该进程集内应用程序的操作系统进程也会收到\ ``EXIT``\ 信号，并且会做一些清理工作，也可能重启进程集。

.. topic:: 程序 8.2

    .. code-block:: erlang

        loop(Processes) ->
	    receive
	        {From, {new_proc, Mod, Func, Args}} ->
		    Id = spawn_link(Mod, Func, Args),
		    From ! {keep_alive, started},
		    loop([{Id, Mod, Func, Args}|Processes]);
		{'EXIT', Id, _} ->
		    case lists:keysearch(Id, 1, Processes) of
		        false ->
			    loop(Processes);
			{value, {Id, Mod, Func, Args}} ->
			    P = lists:delete({Id,Mod,Func,Args},
		    	    Processes),
			    Id1 = spawn_link(Mod, Func, Args),
		    	    loop([{Id1, Mod, Func, Args} | P])
		    end
	    end.

        new_process(Mod, Func, Args) ->
            keep_alive ! {self(), {new_proc, Mod, Func, Args}},
            receive
                {keep_alive, started} ->
                true
            end.

.. figure:: _static/images/8.2.png

   图8.2 操作系统和应用程序进程

.. rubric:: 脚注

.. [#] 即数字\ ``0``\ 到\ ``9``\ 和\ ``*``\ 以及\ ``#``\ 。
.. [#] 这可能是一个好的编程练习，因为它将强制程序的编写者更正这些错误。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
