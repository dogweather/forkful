---
date: 2024-01-20 17:50:33.372087-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1A \u5B57\u7B26\u4E32\u63D2\u503C\
  \u5728 Elixir \u4E2D\u7684\u5B9E\u73B0\u4F7F\u7528\u4E86\u7279\u6B8A\u7684\u8BED\
  \u6CD5 `#{}` \u6765\u5D4C\u5165\u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u3002\u6700\u65E9\
  \u51FA\u73B0\u4E8E\u7C7B\u4F3C Perl \u8FD9\u6837\u7684\u7F16\u7A0B\u8BED\u8A00\uFF0C\
  \u540E\u6765 Ruby \u7684\u5927\u91CF\u4F7F\u7528\u8BA9\u5B83\u53D8\u5F97\u6D41\u884C\
  \u3002Elixir\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.685427-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5B57\u7B26\u4E32\u63D2\u503C\u5728 Elixir\
  \ \u4E2D\u7684\u5B9E\u73B0\u4F7F\u7528\u4E86\u7279\u6B8A\u7684\u8BED\u6CD5 `#{}`\
  \ \u6765\u5D4C\u5165\u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u3002\u6700\u65E9\u51FA\
  \u73B0\u4E8E\u7C7B\u4F3C Perl \u8FD9\u6837\u7684\u7F16\u7A0B\u8BED\u8A00\uFF0C\u540E\
  \u6765 Ruby \u7684\u5927\u91CF\u4F7F\u7528\u8BA9\u5B83\u53D8\u5F97\u6D41\u884C\u3002\
  Elixir \u4F5C\u4E3A\u4E00\u95E8\u73B0\u4EE3\u8BED\u8A00\uFF0C\u7EE7\u627F\u4E86\u8FD9\
  \u4E00\u673A\u5236\uFF0C\u56E0\u4E3A\u5B83\u4E0D\u4EC5\u76F4\u89C2\u800C\u4E14\u6027\
  \u80FD\u4F18\u5F02\u3002\u63D2\u503C\u80CC\u540E\uFF0C\u7F16\u8BD1\u5668\u5B9E\u9645\
  \u4E0A\u4F1A\u62FC\u63A5\u5B57\u7B26\u4E32\u548C\u8868\u8FBE\u5F0F\u7684\u7ED3\u679C\
  ."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
