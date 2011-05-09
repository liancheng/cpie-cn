序言
====

Erlang\ [#]_\ 是由Ericsson and Ellemtel计算机科学实验室的成员为并发分布式系统编程而开发的一种申明式语言。

开发Erlang最初是为了调研现代申明式编程范式是否适用于大型工业电信交换系统的编程。而我们很快便发现适用于电信系统编程的语言也同样适用于大量工业嵌入式实时控制问题。

Erlang的许多原语为大型并发实时系统开发中的一些常见问题提供了解决方案。其\ **模块**\ 系统令大型系统得以分解为粒度更可控的单元。其\ **错误检测**\ 机制用于构建容错软件。其\ **代码加载**\ 原语允许在运行时不停机地升级系统代码\ [#]_\ 。

Erlang具备一套基于进程的并发模型。在这套模型中，并发性是显式的，用户可以精确地控制哪些计算串行哪些计算并行；进程间的消息传递则是异步的，即发送进程一发完消息就立即继续执行。

Erlang进程交换数据的唯一方式就是消息传递。这样一来，应用可以很容易地做到分布式——为单处理器编写的应用可以容易地迁移到多处理器或单处理器网络上。语言中提供的内建机制简化了分布式应用的编写，使得应用既可运行于单机也可运行于计算机网络中。

Erlang的变量具有单次赋值属性\ [#]_\ ——变量一旦被赋值就不可再被更改。该属性对于调试变更中的应用有重要的影响。

程序是完全以\ **函数**\ 的方式编写的——函数的选择通过模式匹配来完成，如此一来程序可以非常的简洁紧凑。

Erlang系统具备内建的时间观念——程序员可以指定一个进程在采取某种行动之前需要等待某条消息多久。这就允许了实时系统的开发。Erlang适用于大多数响应时间为毫秒级的\ **软**\ 实时系统。

有关Erlang的最新信息可通过\ http://www.ericsson.se/erlang\ 获取，e-mail咨询可发送至\ erlang@erix.ericsson.se\ 。

.. raw:: html

    <p style="text-align: right">
      Joe Armstrong<br>
      Robert Virding<br>
      Claes Wikstr&#x0151;m<br>
      Mike Williams<br>
      Computer Science Laboratory<br>
      Erricson Telecommunications Systems Laboratories<br>
      Box 1505<br>
      S-125 25 &#x00c4;lvsj&#x0151;<br>
      Sweden<br>
      <a href="mailto:erlang@erix.ericsson.se">
        <code>erlang@erix.ericsson.se</code>
      </a>
      <br/>
    </p>

.. [#] Agner Krarup Erlang (1878-1929)，丹麦数学家，发展了统计均衡中的随机过程理论——他的理论被广泛地应用于电信业。
.. [#] 这对于电话交换机或空中交通控制系统等嵌入式实时系统非常重要——通常情况下，这些系统是不能因为软件维护而停机的。
.. [#] 也叫做write-once variable或non-destructive assignment。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
