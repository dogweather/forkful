---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

“打印调试输出”是一种编程技术，它允许您在代码执行过程中输出信息。程序员这样做是为了更好地理解和调试代码的运行情况。

## 怎么做:

Elm提供了一个特殊的`Debug.log`函数，允许程序员打印和查看输出。下面是一个简单的示例:

``` Elm
import Debug

main =
    Debug.log "Value of myVar" myVar
```
在此代码中，如果`myVar`的值是`5`, 则控制台的输出将是: "Value of myVar: 5"。

## 深入探讨:

`Debug.log`的历史可以追溯到Elm的早期版本。然而，从Elm 0.19.0版本开始，这个函数被设计为仅用于开发环境，并且在进行--optimize编译时将被移除。

与其替代方法包括使用Elm的`toString`函数和`Html.text`函数打印输出到页面，以更友好的方式展示调试信息。

`Debug.log`函数通过使用JS的`console.log`函数实现，这使得它在控制台输出调试信息成为可能。

## 另请参阅:

- Elm官方文档的Debug模块: https://package.elm-lang.org/packages/elm/core/latest/Debug
- 在控制台打印调试信息的教程: https://guide.elm-lang.org/effects/logging.html
- Elm语言的“toString”函数: https://package.elm-lang.org/packages/elm/core/latest/Basics#toString