---
date: 2024-01-26 03:49:06.037147-07:00
description: "\u4F7F\u7528 Elixir \u7684\u8C03\u8BD5\u5668\u6D89\u53CA\u9010\u6B65\
  \u6267\u884C\u4EE3\u7801\u3001\u68C0\u67E5\u53D8\u91CF\u4EE5\u53CA\u8FFD\u8E2A\u6D41\
  \u7A0B\u4EE5\u6D88\u9664\u9519\u8BEF\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E9B\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u7406\u89E3\u610F\u5916\u60C5\u51B5\u5E76\u786E\u4FDD\
  \u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u6309\u8BBE\u8BA1\u8FD0\u884C\u3002"
lastmod: 2024-02-19 22:05:06.441791
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 Elixir \u7684\u8C03\u8BD5\u5668\u6D89\u53CA\u9010\u6B65\u6267\
  \u884C\u4EE3\u7801\u3001\u68C0\u67E5\u53D8\u91CF\u4EE5\u53CA\u8FFD\u8E2A\u6D41\u7A0B\
  \u4EE5\u6D88\u9664\u9519\u8BEF\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E9B\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u7406\u89E3\u610F\u5916\u60C5\u51B5\u5E76\u786E\u4FDD\u4ED6\
  \u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u6309\u8BBE\u8BA1\u8FD0\u884C\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用 Elixir 的调试器涉及逐步执行代码、检查变量以及追踪流程以消除错误。程序员进行这些操作是为了理解意外情况并确保他们的应用程序按设计运行。

## 如何操作：
Elixir 自带一个名为 `:debugger` 的图形调试器。使用它，你需要启动它并连接到你正在运行的进程。

首先，确保你在一个 `iex` 会话中启动了 `:debugger`：
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

现在，解释你想调试的代码模块：
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

你可以设置一个断点：
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

然后，运行你的函数以触发断点并逐步执行你的代码：
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# 调试器会在带有断点的那一行暂停执行
```

## 深入探究
在 Elixir 的 `:debugger` 之前，Erlang 提供了 Elixir 使用的调试器；它在处理并发进程方面非常强大，这是 Erlang VM（BEAM）的一个优势。与某些其他调试器不同，由于 Elixir 中数据的不可变性，`:debugger` 不允许动态修改变量。至于替代方案，你有 `IEx.pry`，它允许你暂停执行，并在代码中的任何地方跳转到 REPL，这非常方便。

尽管 `:debugger` 适合图形界面，有些人可能更喜欢内置的 `:observer` 工具，它也提供进程检查和系统指标，尽管不专门针对逐步执行代码。Elixir 的社区也贡献了如 `visualixir` 和 `rexbug` 等工具，扩展了调试工具的生态系统，超出了默认工具。

## 另见
- 官方 Elixir 入门指南上的调试：https://elixir-lang.org/getting-started/debugging.html
- Erlang 的 `:debugger` 文档：http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir 论坛上关于调试技巧的讨论：https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
