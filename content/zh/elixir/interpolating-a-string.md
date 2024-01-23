---
title:                "字符串插值"
date:                  2024-01-20T17:50:33.372087-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？

字符串插值是将变量值、表达式或代码片段嵌入字符串中的过程。程序员这么做以动态生成文字内容，用于清晰简洁地构造消息或数据展示。

## How to: 如何操作：

```elixir
name = "小明"
greeting = "你好, #{name}!"
IO.puts greeting
```

输出：

```
你好, 小明!
```

## Deep Dive 深入了解

字符串插值在 Elixir 中的实现使用了特殊的语法 `#{}` 来嵌入变量或表达式。最早出现于类似 Perl 这样的编程语言，后来 Ruby 的大量使用让它变得流行。Elixir 作为一门现代语言，继承了这一机制，因为它不仅直观而且性能优异。插值背后，编译器实际上会拼接字符串和表达式的结果。

除了直接的字符串插值，Elixir 还提供了 `String.Interpolation` 模块以供更为复杂或高级的字符串处理需求。另外，你也可以使用 `IO.inspect/1` 方法直接打印变量或表达式的值作为调试手段。

与直接使用字符串拼接相比，插值通常更快，因为它会在编译时就转换成更优化的形式。

## See Also 更多资源

- Elixir 官方文档: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- 了解 Elixir 插值背后的二进制机制: [http://erlang.org/doc/efficiency_guide/binaryhandling.html](http://erlang.org/doc/efficiency_guide/binaryhandling.html)
- 字符串拼接与插值性能比较: [https://elixirforum.com/t/string-interpolation-vs-concatenation/3148](https://elixirforum.com/t/string-interpolation-vs-concatenation/3148)
