---
title:                "Haskell: 打印调试输出"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么
为什么有时候在编写Haskell程序时，会需要打印调试输出呢？这是因为在程序运行时，我们需要能够跟踪代码的执行过程，以便检查程序是否按照预期工作。打印调试输出可以帮助我们定位问题，并帮助我们更好地理解程序的运行情况。

## 如何做
要在Haskell中打印调试输出，我们可以使用内置的 `Debug.Trace` 模块。该模块提供了 `trace` 函数，它接受一个字符串作为参数，并在程序运行时将该字符串打印到控制台。下面是一个简单的例子：

```Haskell
import Debug.Trace

main = do
  putStrLn "请输入一个数字："
  input <- getLine
  let num = read input :: Int
  trace ("用户输入的数字为：" ++ show num) $ do
    putStrLn "结束程序"
```

运行该程序时，控制台会输出类似以下内容：

```
请输入一个数字：
5
用户输入的数字为：5
结束程序
```

我们可以看到，通过使用 `trace` 函数打印调试输出，我们可以获得程序运行过程中重要的中间结果，从而帮助我们检查程序是否按照预期工作。

## 深入探究
除了简单的字符串，`trace` 函数还可以接受一个函数作为参数。这个函数可以用来计算一个值，然后在打印调试输出时将该值一并显示出来。下面是一个示例代码：

```Haskell
import Debug.Trace

main = do
  putStrLn "请输入一个数字："
  input <- getLine
  let num = read input :: Int
  trace ("用户输入的数字为：" ++ show num) $ do
    let result = trace "计算结果为：" $ num * 2
    putStrLn ("最终结果为：" ++ show result)
```

运行程序后，控制台会输出类似以下内容：

```
请输入一个数字：
5
用户输入的数字为：5
计算结果为：10
最终结果为：10
```

通过这种方式，我们可以在打印调试输出时获得更多的信息，从而帮助我们更好地理解程序的执行过程。

## 参考资料
- [Debug.Trace 文档](https://hackage.haskell.org/package/base-4.15.1.0/docs/Debug-Trace.html)
- [Haskell调试教程](https://wiki.haskell.org/Debugging)

## 另请参阅
- [Haskell 入门指南](http://learnyouahaskell.com/)
- [30分钟Haskell入门](https://sourabhbajaj.com/haskell/)