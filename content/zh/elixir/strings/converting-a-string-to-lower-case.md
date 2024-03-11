---
date: 2024-01-20 17:38:13.530223-07:00
description: "\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u6307\u5C06\u5B57\u7B26\
  \u4E32\u4E2D\u7684\u6240\u6709\u5927\u5199\u5B57\u7B26\u66F4\u6539\u4E3A\u5BF9\u5E94\
  \u7684\u5C0F\u5199\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u4E00\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u6570\u636E\u7684\u4E00\u81F4\u6027\u548C\u6BD4\u8F83\uFF0C\
  \u4EE5\u53CA\u5BF9\u7528\u6237\u8F93\u5165\u7684\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.107455-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u6307\u5C06\u5B57\u7B26\
  \u4E32\u4E2D\u7684\u6240\u6709\u5927\u5199\u5B57\u7B26\u66F4\u6539\u4E3A\u5BF9\u5E94\
  \u7684\u5C0F\u5199\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u4E00\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u6570\u636E\u7684\u4E00\u81F4\u6027\u548C\u6BD4\u8F83\uFF0C\
  \u4EE5\u53CA\u5BF9\u7528\u6237\u8F93\u5165\u7684\u5904\u7406\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
字符串转换为小写指将字符串中的所有大写字符更改为对应的小写字符。程序员执行这一操作是为了数据的一致性和比较，以及对用户输入的处理。

## How to: (如何操作：)
在Elixir中，使用`String.downcase/1`函数将字符串转换为小写。示例如下：

```elixir
# 将字符串转换为小写
iex> String.downcase("Hello, World!")
"hello, world!"

# 使用非英语字符
iex> String.downcase("你好，世界！")
"你好，世界！"

# 空字符串
iex> String.downcase("")
""
```

上面的代码显示了常见的情况：英文字符串、含有非英文字符的字符串和空字符串。

## Deep Dive (深入探讨)
Elixir语言中的`String.downcase/1`函数是根据Unicode标准来实现的。这意味着它可以正确地处理全球范围内的几乎所有语言的字母字符。它不仅限于ASCII字符集。和大多数现代编程语言一样，Elixir处理字符串时是以Unicode为基础进行操作的，确保了它在国际化应用中的广泛适用性。

作为替代，你可能会想到使用Elixir的模式匹配和递归来手动实现小写转换，但这样做通常效率低并且容易出错误。

实现`String.downcase/1`时，Elixir底层调用了Erlang的`:unicode`模块。这个模块会处理字符的大小写转换，并且考虑了语言的特殊规则。例如，德语中的字符"ß"已经是小写，没有对应的大写形式。

## See Also (另请参见)
- Elixir官方文档中的[String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- Unicode标准资料，可从[Unicode Consortium](http://www.unicode.org/)获取
- 关于Elixir字符串处理的更广泛讨论，可阅读Elixir School的[Strings课程](https://elixirschool.com/en/lessons/basics/strings/)
