---
title:                "提取子字符串"
aliases:
- /zh/elixir/extracting-substrings.md
date:                  2024-01-20T17:45:31.086019-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
什么是提取子字符串？就是从一个长字符串中抽出部分内容。为什么程序员这么做？有时候我们只需要信息的片段，比如用户输入，数据分析，或只是简单地展示部分信息。

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
