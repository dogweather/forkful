---
date: 2024-01-26 04:13:08.576459-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u79F0REPL (\u8BFB\u53D6-\u6C42\u503C\
  -\u8F93\u51FA \u5FAA\u73AF)\uFF0C\u5141\u8BB8\u4F60\u5B9E\u65F6\u5C1D\u8BD5\u4EE3\
  \u7801\u7247\u6BB5\u3002Elixir\u7A0B\u5E8F\u5458\u4F7F\u7528\u540D\u4E3AIEx\uFF08\
  \u4EA4\u4E92\u5F0FElixir\uFF09\u7684REPL\u8FDB\u884C\u5B9E\u9A8C\u3001\u8C03\u8BD5\
  \u4EE5\u53CA\u5B66\u4E60\u8BE5\u8BED\u8A00\u3002"
lastmod: 2024-02-19 22:05:06.438524
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u79F0REPL (\u8BFB\u53D6-\u6C42\u503C\
  -\u8F93\u51FA \u5FAA\u73AF)\uFF0C\u5141\u8BB8\u4F60\u5B9E\u65F6\u5C1D\u8BD5\u4EE3\
  \u7801\u7247\u6BB5\u3002Elixir\u7A0B\u5E8F\u5458\u4F7F\u7528\u540D\u4E3AIEx\uFF08\
  \u4EA4\u4E92\u5F0FElixir\uFF09\u7684REPL\u8FDB\u884C\u5B9E\u9A8C\u3001\u8C03\u8BD5\
  \u4EE5\u53CA\u5B66\u4E60\u8BE5\u8BED\u8A00\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
