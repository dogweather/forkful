---
title:                "Elm: 将字符串转换为小写"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么

字符串转换为小写的作用在编程中十分常见。它可以帮助我们统一字符串的大小写，从而方便比较和处理字符串。在 Elm 中，我们可以使用内置的 `String.toLower` 函数来实现这一功能。

# 如何

如果我们想要将一个字符串转换为小写，只需要使用 `String.toLower` 函数，并将需要转换的字符串作为参数传入即可。下面是一个简单的例子：

```elm
import String exposing (toLower)

str = "Hello World"
lowerStr = toLower str

main = 
    text lowerStr -- 输出：hello world
```

在上面的代码中，我们首先导入 `String` 模块，并使用 `toLower` 函数将字符串 `"Hello World"` 转换为小写的 `lowerStr` 字符串。然后在 `main` 函数中，我们使用 `text` 函数将 `lowerStr` 输出到界面上。

需要注意的是，`String.toLower` 函数不会对原始字符串进行任何改变，而是会返回一个新的小写字符串。如果我们想要改变原始字符串的大小写，可以将 `toLower` 的结果赋值给原始字符串变量。

```elm
import String exposing (toLower)

str = "Hello World"
str = toLower str -- 将原始字符串赋值为小写字符串

main = 
    text str -- 输出：hello world
```


# 深入了解

在 Elm 中，字符串实际上是一个字符列表，每个字符都是一个整数表示的 Unicode 码点。当我们使用 `String.toLower` 函数时，它会遍历字符串中的每个字符，并通过 `Char.toLower` 函数将每个字符转换为小写。

除了 `String.toLower` 函数，我们也可以使用 `String.map` 函数来遍历字符串并执行自定义的转换操作。

```elm
import String exposing (..)
import Char exposing (toLower)

str = "Hello World"
lowerStr = map toLower str

main = 
    text lowerStr -- 输出：hello world
```

在上面的代码中，我们导入 `String` 和 `Char` 模块，并使用 `map` 函数遍历字符串 `str` 中的每个字符，并通过 `toLower` 函数将每个字符转换为小写。最后，我们使用 `text` 函数将小写字符串输出到界面上。

# 参考链接

- [Elm 文档 - String 模块](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 文档 - Char 模块](https://package.elm-lang.org/packages/elm/core/latest/Char)
- [Elm 指南 - 字符串操作](https://guide.elm-lang.org/strings/)