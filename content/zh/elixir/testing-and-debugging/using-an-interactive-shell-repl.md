---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- /zh/elixir/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:08.576459-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
交互式shell，或称REPL (读取-求值-输出 循环)，允许你实时尝试代码片段。Elixir程序员使用名为IEx（交互式Elixir）的REPL进行实验、调试以及学习该语言。

## 如何操作：
要启动IEx，打开你的终端并输入`iex`。这里是一个示例：

```Elixir
iex> name = "Elixir 程序员"
"Elixir 程序员"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

输出应展示变量赋值、函数结果以及匿名函数的工作。

## 深入探索
IEx shell自Elixir早期以来就是其一部分。Elixir的创建者José Valim受到了其它语言交互式shell的启发，例如Python的`python`和Ruby的`irb`。虽然IEx与它们有许多共同特点，但它构建于处理Elixir并发性的需要，并与Erlang VM的能力完全集成。

Erlang生态系统中IEx的替代品包括`erl`，即Erlang shell。但IEx提供了一个更友好的Elixir环境，拥有如全面的Tab补全、历史记录及helper等功能。

IEx REPL不仅仅是一个游乐场；它能无缝连接到正在运行的系统。这对于调试实时应用至关重要。其底层实现依赖于BEAM（Erlang虚拟机），确保了在shell中支持热代码交换等特性。

## 另请参阅
查阅以下资源以获取更多阅读材料和资源：

- [Elixir的IEx文档](https://hexdocs.pm/iex/IEx.html)
- [交互式Elixir (IEx) - Elixir的Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlang的`erl`文档](http://erlang.org/doc/man/erl.html)
- [学习Elixir的交互式Shell](https://elixirschool.com/en/lessons/basics/iex_helpers/)
