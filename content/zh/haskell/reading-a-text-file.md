---
title:                "阅读文本文件"
date:                  2024-01-20T17:54:38.046429-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 什么 & 为什么?
读取文本文件就是让程序能够加载和分析存储在文件中的文本数据。程序员这样做是为了处理和转换数据、配置程序，或是从日志里获取信息。

# 怎么做:
```Haskell
import System.IO

main :: IO ()
main = do
    contents <- readFile "example.txt"
    putStr contents
```
假设 `example.txt` 内容是:
```
Hello, Haskell!
```
运行后输出:
```
Hello, Haskell!
```

# 深入了解
在Haskell里，有很多读文件的方法。`readFile` 函数属于预加载 (lazy) 类型，意味着文件内容并非一次性加载，而是在需要时才读取。这和其他语言（如Python的 `open` 或C的 `fread`）不一样。Haskell的 `Data.ByteString` 库提供了一种立即加载 (strict) 方案，可能在处理大文件时更合适。

1990年代早期，Haskell成形时，其输入/输出系统采用了创新的非严格 (non-strict) I/O 模型，并在之后逐渐演变。现如今，我们可以通过 `System.IO` 和 `Prelude` 模块以简洁、高效的方式读取文件。

还有一些替代方案，例如 `hGetContents` 和 `withFile` 函数。这些函数在处理文件打开和关闭时提供更细粒度的控制，适用于更复杂的场景。

# 参考链接
- [Haskell官方文档](https://haskell.org/documentation)
- [Hoogle, a Haskell API search engine](https://hoogle.haskell.org/)
- [Learn You a Haskell for Great Good - Input and Output](http://learnyouahaskell.com/input-and-output)
