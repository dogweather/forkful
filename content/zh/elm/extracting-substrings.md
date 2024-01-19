---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

子字符串提取是从主字符串中获取一部分文本的过程。程序员经常做这个，因为这样可以让我们处理和操作字符串的特定部分。

## 如何做：

以下是 Elm 中提取子字符串的方法：

```Elm
import String

main =
   let
       str = "Hello, World!"
   in
   str
       |> String.slice 0 5
       |> text
-- 输出: "Hello"
```
在这个示例中，`String.slice` 函数把第一位（0）到第五位（5）之间的字符提取出来。

## 深度解读：

1) 历史背景：在早期的编程语言中，提取字符串可能涉及繁琐的内存操作。然而，随着编程语言的发展，这一操作变得更加简化和直接。

2) 可选方案：在一些其他语言中（例如 Python 和 JavaScript），也存在函数（比如 `substring`、`substr`）对字符串进行操作。

3) 实现细节：在Elm中，`String.slice`函数本质上是在安全的操作UTF-16代码点的基础上进行工作，而不是像一些其他语言那样直接处理字节。

## 延伸阅读：

1) [Elm 官方文档：String.slice函数](https://package.elm-lang.org/packages/elm/core/latest/String#slice)

2) [Elm教程：字符串处理](https://elmprogramming.com/strings.html)

3) [字符串处理：理解和实现substring](http://www.computing.dcu.ie/~humphrys/Notes/UNIX/literals.html).