---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:54:23.280188-07:00
description: "\u5982\u4F55: \u4EE5\u4E0B\u662F\u8BA9 Haskell \u8BFB\u53D6\u6587\u672C\
  \u6587\u4EF6\u800C\u4E0D\u8D39\u5439\u7070\u4E4B\u529B\u7684\u65B9\u6CD5\u3002\u6253\
  \u5F00\u4F60\u6700\u559C\u6B22\u7684\u7F16\u8F91\u5668\uFF0C\u6211\u4EEC\u6765\u5199\
  \u4E00\u4E9B\u4EE3\u7801\u3002"
lastmod: '2024-04-04T00:26:53.716333-06:00'
model: gpt-4-0125-preview
summary: "\u4EE5\u4E0B\u662F\u8BA9 Haskell \u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u800C\
  \u4E0D\u8D39\u5439\u7070\u4E4B\u529B\u7684\u65B9\u6CD5\u3002\u6253\u5F00\u4F60\u6700\
  \u559C\u6B22\u7684\u7F16\u8F91\u5668\uFF0C\u6211\u4EEC\u6765\u5199\u4E00\u4E9B\u4EE3\
  \u7801."
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
weight: 22
---

## 如何:
以下是让 Haskell 读取文本文件而不费吹灰之力的方法。打开你最喜欢的编辑器，我们来写一些代码。

```Haskell
import System.IO

main = do
    -- 以读模式打开一个文件
    handle <- openFile "hello.txt" ReadMode
    -- 读取文件内容
    content <- hGetContents handle
    -- 打印文件内容
    putStrLn content
    -- 不要忘记关闭文件句柄！
    hClose handle
```

运行这个程序，如果你有一个包含 "Hello, World!" 的 "hello.txt" 文件，你将得到：

```
Hello, World!
```

这里有一个更短、更酷的方法，做同样的事情但更少的麻烦：

```Haskell
-- 'readFile' 一步完成打开和读取
main = do
    content <- readFile "hello.txt"
    putStrLn content
```

输出仍然是，

```
Hello, World!
```

## 深入探讨
很久以前，程序是不太社会化的生物，主要处理它们自己生成的数据。但随着复杂度的增长，也增长了从外部信息获取数据的需求，因此从文件中读取数据成为了一个基础需求。

Haskell 提供了各种读取文件的方式。我们可以通过“低级”方式使用 `openFile`、`hGetContents` 和 `hClose`，或者使用 `readFile` 来简化操作，后者将所有操作整齐地打包。

`readFile` 是懒惰的 - 它按需读取内容，这对于大型文件来说是内存高效的，但如果文件在读取过程中发生变化，可能会带来意外。低级方法提供了更多的控制，使其更可预测但也更冗长。对于庞大的文本，Haskell 的 `hGetLine` 或像 `conduit` 和 `pipes` 这样的库有助于更精细地管理内存和处理。

Haskell 的标准 `IO` 动作通过底层的操作系统机制来处理文件。这些库将这些操作抽象成更加用户友好的操作，但归根结底，它们都是建立在 Haskell 的 `IO` monad 之上的，确保操作按正确的顺序进行。

## 另请参阅
- 想查看官方 Haskell 文档，请查看 [Haskell 的输入输出文档](https://www.haskell.org/tutorial/io.html)。
- 如果你渴望了解更多，请在 [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output) 品尝知识之杯。
- 通过 [Real World Haskell 的 IO 篇](http://book.realworldhaskell.org/read/io.html) 加深对文件管理的理解。
- 探索用于处理大文件的流式库，例如 [conduit](https://hackage.haskell.org/package/conduit) 和 [pipes](https://hackage.haskell.org/package/pipes)。
