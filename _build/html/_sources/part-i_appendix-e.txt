.. highlight:: erlang
   :linenothreshold: 3

**********
附录E 驱动
**********

:翻译: 连城

本附录描述了如何编写所谓的Erlang内链驱动。任意代码都可以被连入Erlang运行时系统并在某Erlang端口的外端执行。

Erlang进程可以借助端口来收发普通消息。运行时系统与通过端口链入的软件之间通过传递指针来通讯。对于IO非常密集的端口软件来说这样更合适。

.. vim:ft=rst ts=4 sw=4 fenc=utf-8 enc=utf-8 et
