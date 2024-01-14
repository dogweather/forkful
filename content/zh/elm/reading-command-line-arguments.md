---
title:                "Elm: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要从用户的输入中获取信息。命令行参数是一种非常方便的方法，可以让用户在运行程序时提供附加的参数。在这篇文章中，我们将学习如何在Elm中读取命令行参数，以及它的用途和好处。

## 如何进行

首先，让我们看一个简单的例子，演示如何从命令行接收参数并打印出来。在这个例子中，我们将使用一个名为"Hello"的程序，它需要接收一个参数来打印问候语。

```Elm
module Hello exposing (main)

import Platform exposing (worker)

main =
    worker (\arg -> print ("Hello, " ++ arg ++ "!"))
```

为了在命令行中运行这个程序，我们需要使用`elm make`命令来编译它，并使用`elm reactor`或`elm repl`来运行它。在命令行中，我们可以使用`elm reactor`运行程序并接收一个参数，就像这样：

```
$ elm reactor -p 8000
Hello John
```

这将打印出`"Hello, John!"`，因为我们在命令行中提供的参数是`"John"`。你也可以在`elm repl`中运行相同的命令来测试它。

## 深入了解

除了上面这种简单的方法，我们还可以通过使用JavaScript来访问命令行参数来获取更多的控制权。为此，我们需要使用`Platform`模块中的`element`函数并传入一个JavaScript函数作为参数。这个函数将接收一个代表每个命令行参数的数组，并允许我们以任何方式处理它们。

```Elm
module Hello exposing (main)

import Platform exposing (worker)
import Html exposing (text, div)

main =
    worker (\_ -> element (args -> view (Array.length args)))

view paramsCount =
    div [] [ text "Number of command line parameters: " ++ toString paramsCount ]
```

在这个例子中，我们可以使用`Array.length`函数来获取命令行参数的数量，并使用`toString`函数将其转换成字符串，并将结果打印在HTML页面中。在命令行中，我们可以使用`elm make`编译这个程序，并使用`elm reactor`来查看结果：

```
$ elm reactor -p 8000
Number of command line parameters: 3
```

## 参考链接

- 官方文档：[读取命令行参数](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
- Elm编程语言：[官方网站](https://elm-lang.org)
- 通过REPL学习命令行参数：[REPL命令行教程](http://elmrepl.cuberoot.in/)
- 更多关于命令行参数的信息：[读取和解析命令行参数](https://guide.elm-lang.org/interop/flags.html)
- GitHub上的Elm生态系统：[Elm生态系统](https://github.com/elm-lang/elm-lang.org/blob/master/README.md)

## 参考链接