---
title:                "将字符串转化为大写"
html_title:           "Elm: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
大写字符串，意即将字符串中的所有小写字母转换为大写字母。程序员这样做，主要是为了保证文本的一致性和易读性。

## 如何做:
在Elm编程语言中，我们可以使用 `String.toUpper` 函数来实现这个功能。下面给出一段简单的代码示例:
```Elm
import Html exposing (text)
import String

main =
    text (String.toUpper "hello, elm!")
```
运行上述代码，输出结果如下：
```Elm
"HELLO, ELM!"
```
在这个示例中，我们将 "hello, elm!" 这个字符串全部转化为了大写字母。

## 深入探究
大写字符串这个功能很早就出现在编程语言中。而在Elm中， `String.toUpper` 函数的内部实现是借助JS的 `toUpperCase` 函数完成的。

一种可能的替代方式是，逐个遍历字符串中的字符并用 `Char.toUpper` 函数将其变为大写。这种方法虽然更繁琐，但可以让我们更直接地处理每个字符。

在Elm中，并没有现成的函数可以实现只大写字符串中的首字母（像JS中的 `capitalize` 函数那样）。不过，我们可以将字符串拆分为首字母和其余部分，然后分别处理。

## 参见
1. Elm官方文档对 `String.toUpper` 函数的说明：[链接](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
2. Elm官方文档对 `Char.toUpper` 函数的说明：[链接](https://package.elm-lang.org/packages/elm/core/latest/Char#toUpper)