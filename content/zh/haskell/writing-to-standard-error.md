---
title:    "Haskell: 写入标准错误"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为何要将信息写入标准错误

在Haskell编程中，有时候需要将错误信息打印到控制台，以便程序员可以及时发现错误并进行调试。使用标准错误来打印信息比起标准输出更为适合，因为它不会被管道或重定向所影响，从而保证了错误信息的有效传达。

## 如何写入标准错误

要在Haskell中将信息写入标准错误，可以使用hPutStrLn函数。它接受两个参数：句柄和字符串。句柄可以用来指定要写入的目标，而字符串就是要打印的信息。下面是一个例子：

```Haskell
hPutStrLn stderr "这是一个错误信息"
```

运行上面的代码，将会把"这是一个错误信息"打印到控制台的标准错误流中。

## 深入了解标准错误

标准错误是Haskell中一个非常有用的工具，它不仅可以用来打印错误信息，还可以用来打印警告信息或调试信息。通过使用hPutStrLn函数，我们可以很方便地在程序中指定要输出的信息流。此外，标准错误还可以与其他的错误处理机制结合使用，使程序更加健壮。

# 查看更多

- [Haskell标准库文档 - hPutStrLn函数](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)
- [Haskell教程 - 使用标准错误打印信息](https://www.tutorialspoint.com/haskell/haskell_standard_io.htm)
- [Haskell语言参考 - 标准错误](https://www.haskell.org/onlinereport/haskell2010/haskellpa3.html#x8-490003.4)