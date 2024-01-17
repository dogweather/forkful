---
title:                "读取文本文件"
html_title:           "Haskell: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
读取文本文件是指从计算机存储中提取文本信息并将其加载到程序中。程序员在读取文本文件是为了从外部源获取相关数据，然后使用这些数据在程序中进行操作。

# 怎么做：
```Haskell
-- 从文本文件中读取所有内容
main = do
  contents <- readFile "sample.txt" -- 向程序中读取名为"sample.txt"的文本文件
  putStrLn contents -- 将文本文件的内容打印到终端

-- 逐行读取并处理文本文件
main = do
  lines <- readFile "sample.txt" -- 向程序中读取名为"sample.txt"的文本文件
  let numLines = length . lines -- 计算文本文件中行数
  putStrLn $ "This file has " ++ show numLines ++ " lines." -- 将行数打印到终端
```

文件"sample.txt"中的内容：
```
Hello!
This is a sample text file.
It has four lines.
```

输出内容：
```
Hello!
This is a sample text file.
It has four lines.
This file has 4 lines.
```

# 深入探讨：
(1) 阅读文本文件几乎是所有编程语言中都有的一项基本操作，它可以追溯到计算机的早期发展阶段。 (2) 除了使用Haskell内置的`readFile`函数，还可以使用系统命令或第三方库来读取文本文件。 (3) 在读取文本文件时，Haskell会将文件内容储存在内存中，并使用特定的编码来解析文本数据。

# 参考文章：
- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Haskell Wikibooks: Input and Output](https://en.wikibooks.org/wiki/Haskell/Input_and_output)
- [Haskell Wiki: Working with files](https://wiki.haskell.org/Working_with_files)