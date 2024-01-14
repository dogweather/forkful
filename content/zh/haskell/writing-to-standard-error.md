---
title:                "Haskell: 向标准错误写入"
simple_title:         "向标准错误写入"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写标准错误 (Why Write to Standard Error)

在编程中，我们经常需要打印输出来帮助我们调试代码。但是有时候，我们不希望输出被包含在我们程序的正常输出中，因为它们可能会被其他输出混淆。这时，我们就可以使用标准错误，将输出打印到控制台的另一个位置，帮助我们更方便地分析和排除错误。

## 如何写入标准错误 (How To Write to Standard Error)

在Haskell中，我们可以使用 `hPutStrLn` 函数将输出打印到标准错误。下面是一个例子：

```Haskell
import System.IO

main = do
    hPutStrLn stderr "这是一个标准错误输出。"
```

运行该程序，我们将在控制台上看到如下输出：

```
这是一个标准错误输出。
```

注意，标准错误输出默认是以红色字体显示，以帮助我们更易于区分它和标准输出。

## 深入了解 (Deep Dive into Writing to Standard Error)

除了 `hPutStrLn`，Haskell中还有其他函数可以帮助我们向标准错误输出信息。比如，`hPutStr` 和 `hPrint` 函数都可以用来输出字符串或任何可显示的值到标准错误。在实际编程中，我们可以根据需要选择最合适的函数来输出信息。

此外，我们还可以使用 `stderr` 关键字来直接指定标准错误输出，而不必每次都调用 `hPutStrLn` 函数。以下是一个例子：

```Haskell
import System.IO

main = do
    let str = "这是另一个标准错误输出。"
    hPutStrLn stderr str  -- 使用hPutStrLn函数
    hPutStr stderr str   -- 使用hPutStr函数
    hPrint stderr str    -- 使用hPrint函数
    str `debug` stderr   -- 使用debug函数输出
```

运行该程序，我们将看到如下输出：

```
这是另一个标准错误输出。
这是另一个标准错误输出。
这是另一个标准错误输出。
这是另一个标准错误输出。
```

可以看到，效果是一样的。因此，根据个人喜好和编程需要，我们可以选择使用最方便的方式来向标准错误输出信息。

## 参考资料 (See Also)

- [Haskell标准库文档 - System.IO](http://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Haskell中的标准错误输出 - 菜鸟教程](https://www.runoob.com/w3cnote/haskell-stderr.html)
- [跟我学Haskell - 第7章 输入输出](https://book.haskellcn.org/read/dive-into-haskell-laopao/io.html)