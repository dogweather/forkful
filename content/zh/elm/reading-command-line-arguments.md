---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
命令行参数是在启动程序时，通过命令行传递给程序的参数。程序员之所以使用它们，是因为它们对定制程序行为非常有用，允许调整输入，从而改变输出或行为。

## 如何:
在Elm中，您可以使用核心库的`Process.argv`函数来读取命令行参数。这是一个简单的例子：

```Elm
import Process

main =
  Process.argv
    |> List.map toString
    |> List.intercalate " "
    |> Console.log
```

运行`elm make Main.elm && ./Main hello world`，输出将是`hello world`。

## 深度了解:
Elm没有直接访问操作系统底层的能力，包括命令行参数。但是，Elm通过内置的`Process`模块提供了一种方式来读取这些参数。另外，因为Elm强调纯函数，所以它并不鼓励使用命令行参数来改变程序的行为，而更推荐使用明确的输入和输出。

虽然Elm提供了访问命令行参数的方式，但在某些情况下，你可能想要使用更复杂的库，例如`optparse-applicative`，这是一个Haskell库，以更结构化的方式处理命令行参数。

## 参见
- [Elm Process](https://package.elm-lang.org/packages/elm/core/latest/Process)
- [Haskell optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)