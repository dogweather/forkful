---
title:                "用计算机编程：将字符串变大写"
html_title:           "Elm: 用计算机编程：将字符串变大写"
simple_title:         "用计算机编程：将字符串变大写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

大多数编程任务中，我们都需要处理字符串。有时候，我们需要将一个字符串中的所有字母都转换成大写或小写。这时，我们就可以使用 Elm 中的一个函数来实现，它就是 `String.toUpper`。

## 如何使用

下面是一个简单的例子，展示了如何使用 `String.toUpper` 函数来将一个字符串转换为大写：

```Elm
import String exposing (toUpper)

upperCaseString : String -> String
upperCaseString str =
    toUpper str

main =
    upperCaseString "hello world"
```

输出：

```
"HELLO WORLD"
```

## 深入了解

`String.toUpper` 函数的真正作用是将字符串中的每一个字符都转换为大写形式。这意味着它不仅可以用于单词，也可以用于句子中的所有字母。此外，它还可以处理 Unicode 字符，因此即使是非英语字符也可以转换为大写形式。

`String.toUpper` 函数还有一个兄弟函数 `String.toLower`，它将字符串中的所有字符都转换为小写形式。这两个函数非常有用，并且可以帮助我们在处理字符串时节省大量的代码。

## 请参阅

- [Elm 文档](https://guide.elm-lang.org/)
- [String 文档](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Unicode 字符表](https://unicode-table.com/cn/)