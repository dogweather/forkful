---
title:                "字符串大写化"
aliases:
- zh/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:52.598303-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
