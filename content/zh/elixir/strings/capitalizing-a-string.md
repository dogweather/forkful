---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.598303-07:00
description: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\u5B57\
  \u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\
  \u540C\u65F6\u786E\u4FDD\u5176\u4F59\u7684\u5B57\u6BCD\u662F\u5C0F\u5199\u7684\u3002\
  \u8FD9\u79CD\u64CD\u4F5C\u901A\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\u7528\u6237\u8F93\
  \u5165\u6216\u5728\u7528\u6237\u754C\u9762\u4E2D\u663E\u793A\u6587\u672C\uFF0C\u5176\
  \u4E2D\u4E00\u81F4\u6027\u548C\u53EF\u8BFB\u6027\u5F88\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.333084-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\u5B57\
  \u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\
  \u540C\u65F6\u786E\u4FDD\u5176\u4F59\u7684\u5B57\u6BCD\u662F\u5C0F\u5199\u7684\u3002\
  \u8FD9\u79CD\u64CD\u4F5C\u901A\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\u7528\u6237\u8F93\
  \u5165\u6216\u5728\u7528\u6237\u754C\u9762\u4E2D\u663E\u793A\u6587\u672C\uFF0C\u5176\
  \u4E2D\u4E00\u81F4\u6027\u548C\u53EF\u8BFB\u6027\u5F88\u91CD\u8981\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 什么 & 为什么？

字符串首字母大写涉及将字符串的第一个字母转换为大写，同时确保其余的字母是小写的。这种操作通常用于格式化用户输入或在用户界面中显示文本，其中一致性和可读性很重要。

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
