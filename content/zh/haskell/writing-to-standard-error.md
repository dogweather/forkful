---
title:                "将内容写入标准错误"
html_title:           "Haskell: 将内容写入标准错误"
simple_title:         "将内容写入标准错误"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：为什么有人会选择编写标准错误信息？
标准错误信息是编程中非常重要的一种信息输出方式，当程序出现错误时，可以将相关信息输出到标准错误中，帮助程序员快速发现并解决问题。

如何编写标准错误信息：
```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "Oops! Something went wrong."
    hFlush stderr
```

这段代码使用标准库中的System.IO模块来实现将错误信息输出到标准错误中。首先，我们引入该模块，并设置一个main函数。在main函数中，使用hPutStrLn函数将字符串"Oops! Something went wrong."输出到stderr（标准错误）流中。最后，使用hFlush函数确保信息被立即输出。 

深入了解：在Haskell中，标准错误流通常被表示为stderr，它是一个全局变量且是线程不安全的。因此，我们在使用时需要特别小心。另外，除了hPutStrLn和hFlush函数，我们还可以使用hPutStr和hPutChar来分别输出字符串和单个字符到标准错误中。此外，还有一些其他函数可以控制标准错误的行为，比如hDuplicate和hDuplicateTo。详细信息可以在Haskell官方文档中找到。

## 参考链接
- [Haskell官方文档](https://www.haskell.org/documentation)
- [Haskell标准库文档（System.IO模块）](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)