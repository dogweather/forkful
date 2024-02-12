---
title:                "打印调试输出"
aliases: - /zh/haskell/printing-debug-output.md
date:                  2024-01-20T17:52:53.373556-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
调试输出就是在程序运行时显示变量、表达式的值或者程序的状态信息，以帮助程序员理解和解决代码问题。

## How to: (怎么做：)
在Haskell中，我们常用`print`函数输出调试信息。它能自动处理换行，使得输出清晰易读。这里有个示例：

```Haskell
main :: IO ()
main = do
    let numberList = [1..5]
    print numberList
    putStrLn $ "The sum is: " ++ show (sum numberList)
```

输出会是这样的：

```
[1,2,3,4,5]
The sum is: 15
```

## Deep Dive (深入探讨)
以前，`print`语句是调试时的好帮手。现在Haskell使用`print`函数，因为它是一个IO操作。如果想用类似的方式，也可以使用`Debug.Trace`库的`trace`函数。不同的是，它不是IO操作，可以在任意表达式中使用，但不建议在最终程序中使用。

```Haskell
import Debug.Trace

main = trace "This will be printed." $ print (sum [1..5])
```

`trace`将会输出字符串，并返回其第二个参数的值。

在Haskell中，记得区分纯函数和IO函数，因为它们对调试的影响不同。

## See Also (另请参阅)
- Haskell Documentation: https://www.haskell.org/documentation/
- Debug.Trace library: https://hackage.haskell.org/package/base-4.16.0.0/docs/Debug-Trace.html
- Learn You a Haskell for Great Good (a beginner-friendly Haskell tutorial): http://learnyouahaskell.com/
