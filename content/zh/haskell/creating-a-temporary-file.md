---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Haskell 临时文件生成：一种简洁的方法

## 什么 & 为什么？

临时文件创作是为了生成一个可以暂存数据的文件，其在程序结束后会被删除。程序员这样做以便管理那些不再需要持久存储的数据。

## 如何操作:

在Haskell里，我们可以使用 `System.IO.Temp` 库进行临时文件的创建。

```Haskell
import System.IO.Temp (writeSystemTempFile)

main :: IO ()
main = do
    fp <- writeSystemTempFile "tempFile.txt" "This is temporary text"
    print fp
```

在这个示例中，我们创建了一个名为 "tempFile.txt" 的临时文件，并赋予它一些文本内容（"This is temporary text"）。执行后会在控制台输出文件的完整路径。

## 深度解析

临时文件在UNIX和其它早期操作系统上有深远的历史。它们的设计目标就是为了处理短暂存在但是不能放入内存中的数据。现今，几乎所有的编程语言都提供了创建临时文件的方法。

创建临时文件的另一个选择是使用 `System.Directory`，然后手动去删除这个临时文件，但这通常会更复杂，不建议使用。

关于实现细节，`writeSystemTempFile` 函数在内部使用了POSIX函数来确保临时文件的唯一性，并且设置了适当的文件权限，使其只对创建它的进程可见。

## 另见

- [`System.IO.Temp` 文档](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Haskell 文件 I/O 教程](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec)