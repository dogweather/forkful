---
title:                "Elm: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么
提取子字符串是一个非常常见的编程任务，它允许我们从一个字符串中提取部分内容，使得处理和操作更加方便。在Elm中，提取子字符串也是一种常见的操作，它可以帮助我们更有效地处理字符串数据。无论是在开发网页应用还是游戏，子字符串提取都是一个必不可少的技能。

## 如何提取子字符串
在Elm中，我们可以使用内置的 `String.slice` 函数来提取子字符串。它有两个参数，第一个参数是起始索引，第二个参数是结束索引。这两个参数都可以是负数，表示从字符串的末尾开始计算。例如，如果我们想要提取字符串"Hello World"中的 "World"，可以这样做：

```Elm
let
    str = "Hello World"
    result = String.slice 6 -1 str
in
    result -- 输出为 "World"
```

我们也可以使用 `String.left` 和 `String.right` 函数来提取字符串的前几个字符或后几个字符。例如，如果我们想要提取字符串 "Hello World" 中的 "Hello"，我们可以这样做：

```Elm
let
    str = "Hello World"
    result = String.left 5 str
in
    result -- 输出为 "Hello"
```

```Elm
let
    str = "Hello World"
    result = String.right 5 str
in
    result -- 输出为 "World"
```

## 深入了解
除了上面提到的内置函数，我们还可以使用 `List` 模块中的 `take` 和 `drop` 函数来提取字符串的一部分。这两个函数接受一个整数作为参数，表示要提取的字符长度。例如，如果我们想要提取字符串 "Hello World" 中的 "lo"，我们可以这么做：

```Elm
let
    str = "Hello World"
    result = String.toList str |> List.drop 3 |> List.take 2 |> String.fromList
in
    result -- 输出为 "lo"
```

另外，我们也可以使用正则表达式来提取字符串的特定部分。Elm中提供了 `Regex` 模块来操作正则表达式，我们可以使用 `Regex.find` 函数来提取匹配正则表达式的部分。例如，如果我们想要提取字符串 "Hello World" 中的 "World"，我们可以这样做：

```Elm
let
    str = "Hello World"
    pattern = Regex.regex "World"
    result = Regex.find pattern str
in
    case result of
        Just match -> match -- 输出为 "World"
        Nothing -> "" -- 没有匹配到
```

## 参考链接
- Elm官方文档： https://guide.elm-lang.org/strings/
- Elm中的字符串操作： https://package.elm-lang.org/packages/elm/string/latest/
- Elm中的正则表达式操作： https://package.elm-lang.org/packages/elm/regex/latest/