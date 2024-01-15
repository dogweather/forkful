---
title:                "标准错误写入"
html_title:           "Elm: 标准错误写入"
simple_title:         "标准错误写入"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

写入标准错误有什么用？在编程中，错误信息是非常重要的，它可以帮助我们快速找出代码中的问题，并进行调试。如果我们能将错误信息写入到标准错误中，就可以更方便地查看和分析错误，并及时解决它们。

## 如何做

首先，我们需要使用 Elm 语言中的 `Debug` 模块。然后，我们可以使用 `Debug.log` 函数来输出错误信息。下面是一个简单的例子：

```elm
import Debug

x = 5
y = 0

result = x/y

Debug.log "错误信息：" (toString result)
```

在这个例子中，我们定义了两个变量 `x` 和 `y`，然后用 `x` 除以 `y`，由于 `y` 的值为 0，会导致除法错误。最后，我们使用 `Debug.log` 函数来输出错误信息，它的第一个参数为我们自定义的错误信息，第二个参数为要输出的变量。在这个例子中，我们将结果 `result` 转换为字符串，并将它作为第二个参数传入。

运行上述代码后，你会在控制台中看到如下输出：

```
错误信息：Cannot perform division by zero
```

这个错误信息就是我们自定义的，它可以帮助我们快速定位到出错的地方，并找出错误的原因。

## 深入了解

除了 `Debug.log` 函数外，Elm 中还提供了其他几个函数来输出错误信息，比如 `Debug.log2`、`Debug.log3` 等。它们的使用方式与 `Debug.log` 函数类似，只是可以输出不同数量的变量。

另外，我们还可以使用 `Debug.crash` 函数来手动抛出一个错误。它的第一个参数为错误信息，后面可以跟上任意参数。下面是一个例子：

```elm
import Debug

x = 5
y = 0

result = x/y

Debug.crash "错误信息：" result
```

运行上述代码后，你会在控制台中看到如下输出：

```
错误信息：Cannot perform division by zero
```

除了输出错误信息外，`Debug.crash` 函数还会终止程序的执行，这有助于我们在调试时及时发现并解决错误。

## 参考链接

- Elm 官方文档：https://guide.elm-lang.org/
- Elm 语言编程指南（中文版）：http://evancz.gitbooks.io/elm/content/