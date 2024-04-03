---
date: 2024-01-20 17:45:31.086019-07:00
description: "How to: Elixir\u63D0\u4F9B\u4E86\u51E0\u79CD\u65B9\u5F0F\u6765\u63D0\
  \u53D6\u5B50\u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\u51E0\u4E2A\u4F8B\u5B50."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.339772-06:00'
model: gpt-4-1106-preview
summary: "Elixir\u63D0\u4F9B\u4E86\u51E0\u79CD\u65B9\u5F0F\u6765\u63D0\u53D6\u5B50\
  \u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\u51E0\u4E2A\u4F8B\u5B50."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to:
Elixir提供了几种方式来提取子字符串。下面是几个例子:

```elixir
str = "Hello, 世界!"

# 提取固定位置的字符
substring = String.slice(str, 7, 2)
IO.puts(substring) # => 世

# 用模式匹配来提取首次出现的子串
{matched, _rest} = String.split(str, ",")
IO.puts(matched)  # => Hello

# 用正则表达式提取特定模式的子串
regex = ~r/[\u4e00-\u9fff]+/
[match] = Regex.scan(regex, str)
IO.puts(Enum.join(match)) # => 世界
```

注意: 输出可能会因为不同的终端字符集支持而有差异。

## Deep Dive
在Elixir出现之前，提取子字符串通常是手写函数或使用旧版Erlang函数。随着Elixir的发展，增加了对Unicode的支持和正则表达式库，使得提取非英文字符成为可能。现在的实现不仅考虑了效率，也增强了模式匹配的功能，提供更灵活的字符串处理方式。除了`String.slice/3`和`String.split/2`，我们还有`String.starts_with?/2`等函数，提供更多选择。

## See Also
- Elixir官方文档字符串处理: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Unicode正则表达式文档: [https://unicode.org/reports/tr18/](https://unicode.org/reports/tr18/)
- Elixir正则表达式指南: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
