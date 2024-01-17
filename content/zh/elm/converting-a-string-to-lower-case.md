---
title:                "将字符串转换为小写"
html_title:           "Elm: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是字符串转换为小写？

字符串转换为小写是一种在编程中常见的操作，它可以将给定的字符串中的所有字母转换为小写形式。程序员通常会使用这个操作来统一字符串的格式，使其更易于处理和比较。

## 如何进行字符串转换为小写？

在 Elm 中，可以使用 `String.toLower` 函数来将字符串转换为小写形式。以下是一个示例代码段，并展示了其输出结果：

```Elm
import String exposing (toLower)

inputString = "HeLLo WOrld"
outputString = String.toLower inputString
```

输出结果为：`"hello world"`

## 深入探讨

在历史上，字符串转换为小写的操作是为了解决不同编程语言对大小写敏感性的差异。在 Elm 中，也可以使用 `String.toUpper` 函数将字符串转换为大写形式。另外，某些编程语言中也有类似的函数，如 JavaScript 中的 `toLowerCase()`。

要实现字符串转换为小写，通常会遍历字符串中的每个字符，并将大写字母转换为小写。这个操作可以让字符串的比较更加准确，从而避免可能的大小写不匹配问题。

## 查看更多资料

- [Elm 文档](https://elm-lang.org/docs)
- [JavaScript 中的 toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)