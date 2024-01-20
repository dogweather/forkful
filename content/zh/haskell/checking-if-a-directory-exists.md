---
title:                "检查目录是否存在"
html_title:           "Haskell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么与为什么?
检查目录是否存在是操作系统编程中常见的一个步骤，它的主要目的是确定给定的路径是否对应到一个有效的文件夹。这一点对于需要访问、写入或者读取文件夹中文件的程序非常重要，如果目录不存在或者不能访问，这样的操作通常会导致错误。

## 如何做
在Haskell中，我们可以使用`System.Directory`包完成这项工作。下面是一个简单的示例：

```Haskell
import System.Directory

main :: IO ()
main = do
    putStrLn "请输入一个目录路径:"
    path <- getLine
    isExist <- doesDirectoryExist path
    if isExist
        then putStrLn $ "目录 " ++ path ++ " 存在。"
        else putStrLn $ "目录 " ++ path ++ " 并不存在。"
```

运行上述代码，它会提示您输入一个目录路径，然后检查此路径是否存在并返回结果。

## 深入探讨
在早期的编程中，目录的存在性检查并不是常见的操作，可能是因为早期的系统并没有提供太多的文件管理功能。但是，随着操作系统的复杂性和功能的丰富，检查目录是否存在变得越来越重要。

和`doesDirectoryExist`函数类似，`System.Directory`模块中还有一个名为`doesFileExist`的函数，它可以用来检查一个文件是否存在。

`doesDirectoryExist`函数底层通过系统调用实现，因此其性能消耗相对较小。尽管如此，大量频繁的存在性检查可能会产生不小的影响，因此，只有在必要时才进行这样的操作。

## 另请参见
以下是一些相关的学习资源，帮助您更深入理解这个主题：
1. Haskell官方文档：[System.Directory](http://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
3. Learn You a Haskell for Great Good：[IO](http://learnyouahaskell.com/input-and-output)