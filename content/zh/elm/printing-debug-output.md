---
title:                "Elm: 打印调试输出"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出(Debug Output)

打印调试输出是 Elm 编程中非常有用的技术，它可以让开发者更加轻松地调试代码并找到潜在的 bug。通过打印各种变量和表达式的值，我们可以更加清晰地了解代码的执行流程，从而快速定位问题所在。

## 如何使用(Debug Output)

要打印调试输出，我们需要使用 Elm 提供的 `Debug` 模块。让我们来看一个简单的例子：

```Elm
import Debug

sum : Int -> Int -> Int
sum x y =
    Debug.log ("Calculating sum of " ++ toString x ++ " and " ++ toString y) (x + y)

```

上面的代码中，我们定义了一个简单的 `sum` 函数，并使用 `Debug.log` 函数打印了计算的过程和结果。在运行程序时，我们会在控制台看到类似以下的输出：

```
(3) Calculating sum of 1 and 2
(3) 3
```

从上面的输出中，我们可以清楚地看到程序一步步执行的过程以及最终的结果。这样的调试输出可以帮助我们更快地找到代码中的问题，并且在开发过程中提高效率。

## 深入了解(Debug Output)

除了简单的打印变量和表达式的值，我们还可以使用 `Debug.log` 函数来打印自定义的消息，帮助我们更好地理解代码的逻辑。此外，Elm 提供了另一个函数 `Debug.toString` 用于将任意值转换为字符串，这样我们就可以打印自定义类型的数据。

除了 `Debug.log`，我们还可以使用 `Debug.todo` 函数来标记未完成的代码。这可以帮助我们在开发过程中先标记出需要实现的部分，然后再回过头来完成它们。

## 参考文章(See Also)

- [Elm 官方文档中关于 `Debug` 模块的介绍](https://guide.elm-lang.org/debugging/debug.html)
- [如何使用 Elm 来打印调试输出的详细教程](https://dev.to/eriktimothyjordan/elm-debugging-4inn)
- [使用 Elm 调试工具来寻找潜在的性能问题](https://www.lucamug.dev/2019/04/you-probably-need-that-elm-debug-tool.html)