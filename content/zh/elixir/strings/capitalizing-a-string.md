---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.598303-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir \u63D0\u4F9B\u4E86\u4E00\u79CD\
  \u7B80\u5355\u76F4\u63A5\u7684\u65B9\u6CD5\uFF0C\u4F7F\u7528\u5176\u5185\u7F6E\u51FD\
  \u6570\u5C31\u53EF\u4EE5\u5B9E\u73B0\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\
  \uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u7B80\
  \u5355\u7684\u4F8B\u5B50\uFF1A."
lastmod: '2024-03-13T22:44:47.333084-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u63D0\u4F9B\u4E86\u4E00\u79CD\u7B80\u5355\u76F4\u63A5\u7684\u65B9\
  \u6CD5\uFF0C\u4F7F\u7528\u5176\u5185\u7F6E\u51FD\u6570\u5C31\u53EF\u4EE5\u5B9E\u73B0\
  \u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\
  \u5E93\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
Elixir 提供了一种简单直接的方法，使用其内置函数就可以实现字符串首字母大写，无需第三方库。以下是一个简单的例子：

```elixir
string = "elixir 编程"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

输出：

```
Elixir 编程
```

对于需要更多控制或更复杂的大写逻辑的情况，你可能会结合使用不同的 String 函数。例如，如果你想将一个句子中的每个单词都大写，你可以将句子分割成单词，大写每个单词，然后再将它们连接起来：

```elixir
sentence = "elixir 很好玩"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

输出：

```
Elixir 很好玩
```

虽然 Elixir 的标准库涵盖了大多数需求，但对于更细致的文本操作，包括高级字符串大写，你可能会探索第三方库，如 Cldr 用于国际化，这可以提供特定于地域的大写行为。
