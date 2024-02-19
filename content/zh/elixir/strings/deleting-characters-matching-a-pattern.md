---
aliases:
- /zh/elixir/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:41:49.786516-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5220\u9664\u5B57\u7B26\u610F\u5473\u7740\
  \u6839\u636E\u7279\u5B9A\u6A21\u5F0F\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u4E00\
  \u4E9B\u5143\u7D20\u3002\u8FD9\u901A\u5E38\u7528\u4E8E\u6570\u636E\u6E05\u6D17\uFF0C\
  \u6BD4\u5982\u53BB\u9664\u65E0\u7528\u7684\u7A7A\u683C\u3001\u6362\u884C\u7B26\u6216\
  \u8005\u975E\u6CD5\u5B57\u7B26\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.852098
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5220\u9664\u5B57\u7B26\u610F\u5473\u7740\
  \u6839\u636E\u7279\u5B9A\u6A21\u5F0F\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u4E00\
  \u4E9B\u5143\u7D20\u3002\u8FD9\u901A\u5E38\u7528\u4E8E\u6570\u636E\u6E05\u6D17\uFF0C\
  \u6BD4\u5982\u53BB\u9664\u65E0\u7528\u7684\u7A7A\u683C\u3001\u6362\u884C\u7B26\u6216\
  \u8005\u975E\u6CD5\u5B57\u7B26\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
在编程中，删除字符意味着根据特定模式移除字符串中的一些元素。这通常用于数据清洗，比如去除无用的空格、换行符或者非法字符。

## How to: (如何操作：)
Elixir provides the `String` module which contains functions to handle string manipulations. Here's a simple example of deleting characters:

```elixir
# 删除字符串中所有的数字
string = "My phone number is 123-456-7890."
clean_string = String.replace(string, ~r/\d+/, "")
IO.puts(clean_string)
# Output: My phone number is --.
```

Or if you want to strip whitespace from a string, you can do this:

```elixir
# 删除字符串前后的空格
string = "   Hello, World!   "
trimmed_string = String.trim(string)
IO.puts(trimmed_string)
# Output: Hello, World!
```

## Deep Dive (深入了解)
Removing characters matching a pattern in Elixir is often done using regular expressions (regex). Regex is a powerful pattern-matching language that's been around for decades. While Elixir's `String` module is quite robust, alternatives such as `Regex` module or list comprehensions could also be used for more complex patterns.

The Elixir standard library leverages the high performance of Erlang's underlying regular expression library, which is based on Perl's regular expression library. This makes operations like matching and replacing highly efficient in Elixir.

Besides performance considerations, thoughtfully choosing regexp patterns is crucial as poorly designed patterns can lead to inefficient or even incorrect code.

## See Also (另请参阅)
- Elixir's `String` module documentation: https://hexdocs.pm/elixir/String.html
- `Regex` module documentation: https://hexdocs.pm/elixir/Regex.html
- Interactive Elixir regex tool: https://regexr.com/ - While it's not Elixir-specific, it's helpful for testing regex patterns that can be used in Elixir.
