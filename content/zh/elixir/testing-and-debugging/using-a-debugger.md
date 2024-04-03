---
date: 2024-01-26 03:49:06.037147-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir \u81EA\u5E26\u4E00\u4E2A\u540D\
  \u4E3A `:debugger` \u7684\u56FE\u5F62\u8C03\u8BD5\u5668\u3002\u4F7F\u7528\u5B83\uFF0C\
  \u4F60\u9700\u8981\u542F\u52A8\u5B83\u5E76\u8FDE\u63A5\u5230\u4F60\u6B63\u5728\u8FD0\
  \u884C\u7684\u8FDB\u7A0B\u3002 \u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5728\u4E00\u4E2A\
  \ `iex` \u4F1A\u8BDD\u4E2D\u542F\u52A8\u4E86 `:debugger`\uFF1A."
lastmod: '2024-03-13T22:44:47.370740-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u81EA\u5E26\u4E00\u4E2A\u540D\u4E3A `:debugger` \u7684\u56FE\u5F62\
  \u8C03\u8BD5\u5668\u3002\u4F7F\u7528\u5B83\uFF0C\u4F60\u9700\u8981\u542F\u52A8\u5B83\
  \u5E76\u8FDE\u63A5\u5230\u4F60\u6B63\u5728\u8FD0\u884C\u7684\u8FDB\u7A0B."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
