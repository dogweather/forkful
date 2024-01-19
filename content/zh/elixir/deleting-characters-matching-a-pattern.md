---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 是什么？为什么？
在编程中，删除匹配模式的字符是一种常见的字符串操作。通过找到匹配特定模式的子字符串并将其替换为其他字符串或从主字符串中移除，程序员可以处理和操作数据。

## 如何操作
在 Elixir 中，可以使用 `String.replace/3` 函数从字符串中删除匹配模式的字符。使用正则表达式进行匹配。

```elixir
IO.puts String.replace("Hello, World!", ~r/[aeiou]/, "")
```

输出：

```elixir
"Hll, Wrld!"
```

上述代码将所有元音 (a, e, i, o, u) 从字符串 "Hello, World!" 中删除。

## 深入了解
Elixir 的 `String.replace/3` 函数遵循了来自于它所依赖的 ErLang runtime 的正则表达式标准。然而，正则表达式并非唯一的解决方案。你也可以通过 `String.split/2`函数来制定一个模式，然后通过 `Enum.join/2` 函数将结果连接。 

举个例子：

```elixir
IO.puts Enum.join(String.split("Hello, World!", ~r/[aeiou]/), "")
```

输出：

```elixir
"Hll, Wrld!"
```

## 参见
1. [Elixir String Functions](https://hexdocs.pm/elixir/String.html)
2. [Elixir Regular Expressions](https://hexdocs.pm/elixir/Regex.html)
3. [Elixir Enumerables](https://hexdocs.pm/elixir/Enum.html)