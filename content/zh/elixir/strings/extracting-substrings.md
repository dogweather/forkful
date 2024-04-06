---
date: 2024-01-20 17:45:31.086019-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.566844-06:00'
model: gpt-4-1106-preview
summary: "\u5728Elixir\u51FA\u73B0\u4E4B\u524D\uFF0C\u63D0\u53D6\u5B50\u5B57\u7B26\
  \u4E32\u901A\u5E38\u662F\u624B\u5199\u51FD\u6570\u6216\u4F7F\u7528\u65E7\u7248Erlang\u51FD\
  \u6570\u3002\u968F\u7740Elixir\u7684\u53D1\u5C55\uFF0C\u589E\u52A0\u4E86\u5BF9Unicode\u7684\
  \u652F\u6301\u548C\u6B63\u5219\u8868\u8FBE\u5F0F\u5E93\uFF0C\u4F7F\u5F97\u63D0\u53D6\
  \u975E\u82F1\u6587\u5B57\u7B26\u6210\u4E3A\u53EF\u80FD\u3002\u73B0\u5728\u7684\u5B9E\
  \u73B0\u4E0D\u4EC5\u8003\u8651\u4E86\u6548\u7387\uFF0C\u4E5F\u589E\u5F3A\u4E86\u6A21\
  \u5F0F\u5339\u914D\u7684\u529F\u80FD\uFF0C\u63D0\u4F9B\u66F4\u7075\u6D3B\u7684\u5B57\
  \u7B26\u4E32\u5904\u7406\u65B9\u5F0F\u3002\u9664\u4E86`String.slice/3`\u548C`String.split/2`\uFF0C\
  \u6211\u4EEC\u8FD8\u6709`String.starts_with?/2`\u7B49\u51FD\u6570\uFF0C\u63D0\u4F9B\
  \u66F4\u591A\u9009\u62E9\u3002"
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
