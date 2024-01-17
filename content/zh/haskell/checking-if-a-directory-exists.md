---
title:                "检查目录是否存在。"
html_title:           "Haskell: 检查目录是否存在。"
simple_title:         "检查目录是否存在。"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么是检查目录是否存在？为什么程序员要这样做？

检查目录是否存在是指在编程过程中，我们想要确定一个指定的目录是否存在。程序员经常做这样的检查来避免因为找不到目录而导致的程序错误。

## 怎么做？

通过使用 `doesDirectoryExist` 函数可以检查指定的目录是否存在。它的用法如下所示：

```Haskell
import System.Directory

main = do
  let path = "/usr/bin"
  exists <- doesDirectoryExist path
  if exists
    then putStrLn $ "目录 " ++ path ++ " 存在"
    else putStrLn $ "目录 " ++ path ++ " 不存在"
```

输出结果为：

```
目录 /usr/bin 存在
```

## 深入探讨

- 历史背景：在早期的编程语言中，检查目录是否存在的功能并不常见。随着计算机系统变得更加复杂，检查目录是否存在成为了必要的步骤。
- 替代方法：除了 `doesDirectoryExist` 函数外，还有其他方式来检查目录是否存在。例如，可以使用 `getDirectoryContents` 函数来获取当前目录下的所有文件和目录，然后再判断指定的目录是否在其中。
- 实现细节：在 Haskell 中，`doesDirectoryExist` 函数是通过 `getDirectoryStatus` 函数实现的。这个函数会返回一个 `FileStatus` 类型的值，包含了文件或目录的详细信息，比如文件大小、修改时间等。然后通过检查 `fileMode` 字段中是否包含 `Directory` 标志来判断目录是否存在。

## 参考资料

- [Haskell Directory 模块文档](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell System.Posix.Files 模块文档](https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html)