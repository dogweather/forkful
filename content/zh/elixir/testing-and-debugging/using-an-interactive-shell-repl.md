---
date: 2024-01-26 04:13:08.576459-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A IEx shell\u81EAElixir\u65E9\u671F\u4EE5\
  \u6765\u5C31\u662F\u5176\u4E00\u90E8\u5206\u3002Elixir\u7684\u521B\u5EFA\u8005Jos\xE9\
  \ Valim\u53D7\u5230\u4E86\u5176\u5B83\u8BED\u8A00\u4EA4\u4E92\u5F0Fshell\u7684\u542F\
  \u53D1\uFF0C\u4F8B\u5982Python\u7684`python`\u548CRuby\u7684`irb`\u3002\u867D\u7136\
  IEx\u4E0E\u5B83\u4EEC\u6709\u8BB8\u591A\u5171\u540C\u7279\u70B9\uFF0C\u4F46\u5B83\
  \u6784\u5EFA\u4E8E\u5904\u7406Elixir\u5E76\u53D1\u6027\u7684\u9700\u8981\uFF0C\u5E76\
  \u4E0EErlang\u2026"
lastmod: '2024-04-05T22:51:00.581856-06:00'
model: gpt-4-0125-preview
summary: "Erlang\u751F\u6001\u7CFB\u7EDF\u4E2DIEx\u7684\u66FF\u4EE3\u54C1\u5305\u62EC\
  `erl`\uFF0C\u5373Erlang shell\u3002\u4F46IEx\u63D0\u4F9B\u4E86\u4E00\u4E2A\u66F4\
  \u53CB\u597D\u7684Elixir\u73AF\u5883\uFF0C\u62E5\u6709\u5982\u5168\u9762\u7684Tab\u8865\
  \u5168\u3001\u5386\u53F2\u8BB0\u5F55\u53CAhelper\u7B49\u529F\u80FD\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
