---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，提取子字符串就是从一个更大的字符串中选取特定的字符序列。这常常用于处理字符串数据，这需要我们以更小单元进行操作或分析。

## 如何执行：

在Elixir中，我们可以使用`String.slice/3`函数来提取字符串。看下面的例子：

```Elixir
our_string = "程序编程在世界范围内是非常重要的！"
String.slice(our_string, 0, 4)
```

运行以上代码，会输出：

```Elixir
"程序编程"
```

还可以用负数来提取字符串的尾部：

```Elixir
String.slice(our_string, -6, 4)
```

运行以上代码，你将得到：

```Elixir
"是非常重"
```

## 深度了解

从历史上看，提取子字符串一直是处理和操作字符串数据不可或缺的一部分。历史上的一些编程语言，如Python和JavaScript，都有内置函数来处理子字符串的提取。

在Elixir中，除了`String.slice/3`函数，还有附加的`binary_part/3`函数，这是Erlang内核提供的内置函数，其可以用于任何二进制数据，包括字符串。

在选择哪个函数来提取子字符串时，需要考虑你下一步将要做什么，因为`String.slice`的行为和`binary_part`在不同语境下的行为可能略有差异——`binary_part`只考虑字节数，而`String.slice`考虑代码点。

## 另请参阅

1. [`String.slice/3` Elixir官方文档](https://hexdocs.pm/elixir/String.html#slice/2)
2. [`binary_part/3` Erlang官方文档](https://www.erlang.org/doc/man/binary.html#part-3)