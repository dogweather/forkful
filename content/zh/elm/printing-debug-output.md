---
title:    "Elm: 打印调试输出"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，如果程序出现错误或者bug，打印调试信息是一种常见的解决方法。它可以帮助开发者定位问题并且更快地修复错误。在Elm编程中，打印调试信息也是一种重要的技巧，能够使我们更有效地调试程序。

## 如何

当程序出现错误时，我们可以使用`debug`函数来打印调试信息。例如，我们有一个函数来检查一个数字是否为偶数：

```Elm
isEven : Int -> Bool
isEven n =
    if modBy 2 n == 0 then
        True
    else
        False
```
如果我们想要确认这个函数是否按照预期工作，我们可以在其中添加打印语句：

```Elm
isEven : Int -> Bool
isEven n =
    if modBy 2 n == 0 then
        True
    else
        (debug "Input:" n) False
```
这样，当我们调用这个函数时，会在控制台输出一条信息："Input: 5"。通过这个信息，我们可以确认函数的输入值是否正确，并且观察程序在哪里出现问题。

## 深入了解

除了打印简单的文本信息外，Elm 还提供了很多有用的调试函数。例如，`log`函数可以打印任何类型的值，`identity`函数可以返回任何输入值，`trace`函数可以在控制台打印当前函数或者值的信息。在实际开发中，我们可以根据具体情况使用不同的调试函数来帮助我们调试程序。

## 参考资料

- [Elm调试文档](https://guide.elm-lang.org/debugging/)
- [更多关于Elm调试技巧](https://reactnative.cn/docs/debugging.html)
- [实战中的Elm调试技巧](https://github.com/idkjs/debugger)