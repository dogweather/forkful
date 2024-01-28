---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:14:46.592397-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

REPL，即读取-求值-打印循环（Read-Eval-Print Loop）的简称，是一种编程工具，可用于交互式地运行代码并即时看到结果。程序员使用它来实验、调试或像Gleam这样的新语言即兴学习。

## 如何操作：

Gleam目前在其标准发行版中不包含REPL。然而，你可以使用现有的Erlang shell来试验Gleam代码，因为Gleam编译为Erlang字节码。操作方法如下：

1. 将你的Gleam代码编译为Erlang。
```plaintext
gleam build
```

2. 启动Erlang shell。
```plaintext
erl -pa ebin
```

3. 调用你的Gleam函数（假设你有一个名为`my_mod`的模块和一个函数`my_fun`）。
```erlang
my_mod:my_fun().
```

你应该会在shell中看到你的函数输出。

## 深入探讨

REPL体现了许多函数式编程语言的动态性和探索性精神，可以追溯到1960年代LISP的REPL。相比之下，Python的`ipython`或Ruby的`irb`等系统为它们的社区提供了类似的体验。

虽然Gleam还没有原生REPL，但利用Erlang shell仍是一个巧妙的解决办法。Erlang shell的能力来自于BEAM VM，这是一个虚拟机，为Erlang生态系统提供动力，包括Elixir、LFE和Gleam。

Gleam生态系统中REPL的替代品可能包括编写测试用例或使用支持Gleam的在线编译器和代码游乐场，在完整的项目设置外测试代码片段。

实施一个专门的Gleam REPL面临的主要挑战在于Gleam和Erlang的运行时的编译性质，其中热代码替换是常态。任何未来的Gleam REPL都需要解决语言的静态类型与REPL期望的动态执行环境之间的矛盾。

## 另请参阅

- Gleam的官方文档：https://gleam.run/book/
- Erlang shell的文档：http://erlang.org/doc/man/erl.html
- 一个在线的Gleam编译器游乐场：https://gleam.run/compiler/
