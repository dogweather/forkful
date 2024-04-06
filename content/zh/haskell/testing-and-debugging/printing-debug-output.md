---
date: 2024-01-20 17:52:53.373556-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u5728Haskell\u4E2D\uFF0C\u6211\u4EEC\
  \u5E38\u7528`print`\u51FD\u6570\u8F93\u51FA\u8C03\u8BD5\u4FE1\u606F\u3002\u5B83\u80FD\
  \u81EA\u52A8\u5904\u7406\u6362\u884C\uFF0C\u4F7F\u5F97\u8F93\u51FA\u6E05\u6670\u6613\
  \u8BFB\u3002\u8FD9\u91CC\u6709\u4E2A\u793A\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.130439-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u5728Haskell\u4E2D\uFF0C\u6211\u4EEC\u5E38\u7528\
  `print`\u51FD\u6570\u8F93\u51FA\u8C03\u8BD5\u4FE1\u606F\u3002\u5B83\u80FD\u81EA\u52A8\
  \u5904\u7406\u6362\u884C\uFF0C\u4F7F\u5F97\u8F93\u51FA\u6E05\u6670\u6613\u8BFB\u3002\
  \u8FD9\u91CC\u6709\u4E2A\u793A\u4F8B\uFF1A."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
