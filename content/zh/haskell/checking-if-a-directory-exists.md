---
title:                "Haskell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编写Haskell程序时，有时我们需要检查某个目录是否存在。这可能是因为我们需要读取或写入该目录下的文件，或者我们想要确保程序在运行时不会遇到错误。

## 如何进行

要检查目录是否存在，我们可以使用`doesDirectoryExist`函数。这个函数接受一个字符串作为参数，该字符串是要检查的目录的路径。如果目录存在，则函数返回`True`，否则返回`False`。

```Haskell
import System.Directory (doesDirectoryExist)

main = do
    result <- doesDirectoryExist "/path/to/directory"
    print result
```

如果`/path/to/directory`是一个存在的目录，则上面的程序将输出`True`，否则输出`False`。

## 深入探讨

下面是一些有用的信息，帮助你更深入地理解如何检查目录是否存在：

- `System.Directory`模块还提供了其他函数来操作目录，比如`createDirectory`和`removeDirectory`。
- 使用`System.FilePath`模块中的函数来构建跨平台兼容的目录路径。
- 如果需要检查文件是否存在，可以使用`doesFileExist`函数。

## 参考阅读

- [Haskell官方文档：System.Directory](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/directory-1.3.6.0/System-Directory.html)
- [Haskell官方文档：System.FilePath](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/filepath-1.4.2.1/System-FilePath.html)
- [使用Haskell操作文件和目录](https://mmhaskell.com/blog/2018/6/27/taking-advantage-of-lazy-io-in-haskell)