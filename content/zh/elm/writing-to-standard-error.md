---
title:                "写作标准错误"
html_title:           "Elm: 写作标准错误"
simple_title:         "写作标准错误"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误输出？为什么程序员要这样做？

标准错误输出是一种将错误信息打印到屏幕而不是输出到程序控制台的方法。程序员经常在调试代码时使用它来显示程序运行过程中的错误信息，以便更容易地找出并解决问题。

## 如何操作：

```Elm
import Html exposing (text)
import Debug exposing (crash)

main =
  text "Hello World!"

main2 =
  crash "Oops, something went wrong!"
```

输出：

```
Oops, something went wrong!
```

以上代码演示了如何使用标准错误输出来排除错误。通过使用 `Debug.crash` 函数，我们可以将错误信息打印到屏幕上，以便程序员可以更轻松地调试代码。

## 深入了解：

标准错误输出最初是在Unix系统中使用的一种错误处理方式。它与其他处理错误的方式相比具有更高的灵活性。在一些编程语言中，程序员可以使用 `stderr` 写入标准错误输出。除了 `Debug.crash` 函数，还有一些其他的方法来打印错误信息到标准错误输出，比如 `Task.attempt` 函数。

## 相关链接：

- [Elm语言官方网站](https://elm-lang.org/)
- [标准错误输出的历史发展](https://en.wikipedia.org/wiki/Standard_error)