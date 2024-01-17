---
title:                "插值字符串"
html_title:           "Elixir: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串内插是指在字符串中插入变量或表达式的过程。程序员经常使用字符串内插，因为它可以让代码更简洁、清晰明了，同时也提高了速度和可读性。

## 如何：
Elixir中的字符串内插非常简单，只需要用双引号包裹字符串，然后在字符串中插入变量或表达式，如下所示：
```Elixir
name = "Alice"
"Hello #{name}!" #=> "Hello Alice!"
```

## 深入探讨：
在Elixir的前身Erlang中也有字符串内插的概念，但语法略有不同。在Elixir中，使用#{}来包裹变量或表达式，而Erlang中是使用~p来标记。另外，如果需要在字符串中插入多个变量或表达式，可以使用逗号将它们隔开。

## 更多信息：
- [“Elixir字符串内插”官方文档](https://hexdocs.pm/elixir/master/String.html#module-string-interpolation)
- [LearnElixir](https://www.learnelixir.com/string-interpolation)
- [String Interpolation in Elixir - Kevin Old's Blog](https://blog.kevinold.com/2018/12/02/string-interpolation-in-elixir/)