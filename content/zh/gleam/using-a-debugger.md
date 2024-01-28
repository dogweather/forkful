---
title:                "使用调试器"
date:                  2024-01-26T03:49:45.877270-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用调试器基本上就是你在代码中扮演侦探，寻找漏洞并弄清楚为什么事情不能顺利运行。程序员之所以要这么做，是因为，面对它，漏洞是不可避免的，高效地消除它们意味着让你的代码更快、更可靠地运行。

## 如何操作：
Gleam 目前依靠 Erlang 生态系统进行工具支持，因此你通常会使用像 `rebar3`、`observer` 和 `debugger` 这样的工具进行调试。以下是如何深入调试的方法：

```gleam
// 在你的 rebar 配置中，确保你有这些行来包含调试信息：
{erl_opts, [debug_info]}.

// 运行一个带有你的应用程序的 Erlang shell
rebar3 shell

// 在 shell 内部，你可以启动调试器
1> debugger:start().
```

简单，对吧？`debugger` 图形界面弹出，你可以设置断点，逐步执行代码，并按照你的意愿观察变量。你不会直接看到 Gleam 代码，但可以看到它编译成的 Erlang 代码，这仍然非常有帮助。

## 深入探索
Gleam 是一门年轻的语言，因此尽管它依赖于 Erlang 生态系统，但原生的 Gleam 调试工具还没有成为焦点。这意味着我们正在使用 Erlang 的经过验证的工具，这并不是坏事。Erlang 的调试器自 90 年代以来就存在，经过多年的改进，用于消除系统中可靠性至关重要的烦人漏洞。

至于替代方案，跟踪是 BEAM 世界（即运行 Erlang 和 Elixir 代码的虚拟机）中的一种强大方法。使用 `rebar3`，你可以利用像 `recon` 这样的工具来跟踪函数调用，并深入研究性能问题。

在编写 Gleam 和用 Erlang 调试之间的切换可能会感觉像是你在即时翻译你的思维。但好处是你可以窥见 Erlang 世界，理解应用程序在其运行时形式中的构建块。

## 另请参阅
要扩展你的调试工具箱，请查看：

- Erlang 的调试文档：[https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Erlang 的 `recon` 库：[https://ferd.github.io/recon/](https://ferd.github.io/recon/)
