---
title:                "连接字符串"
html_title:           "PowerShell: 连接字符串"
simple_title:         "连接字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么是字符串连接？

字符串连接是将多个字符串合并成一个新的字符串的过程。程序员经常使用这种技术来创建包含动态内容的文本，比如在打印输出中添加变量值或生成复杂的文本信息。字符串连接也可以用来构造数据库查询语句或文件路径。

## 如何实现字符串连接？

使用 PowerShell，你可以通过使用运算符 `+` 将两个字符串连接起来。下面的示例将两个字符串合并成一个新的字符串，并打印输出结果。

```
    $str1 = "Hello"
    $str2 = "World"
    $result = $str1 + $str2
    PowerShell -Command {Write-Host $result}
    # Output: HelloWorld
```

你也可以使用内置的 `Join-Path`函数来连接更复杂的文件路径。下面的示例将 `C:\Users` 路径和 `Documents` 文件夹连接起来，并打印输出结果。

```
    $path = Join-Path -Path C:\Users -ChildPath Documents
    PowerShell -Command {Write-Host $path}
    # Output: C:\Users\Documents
```

## 深入了解

历史背景：
字符串连接是一种程序设计中常用的技巧，它最早出现在早期的编程语言中，比如 BASIC 和 COBOL。随着计算机技术的发展，字符串连接也逐渐被纳入到更加强大和灵活的编程语言中，比如 PowerShell。

替代方法：
除了使用 `+` 运算符和 `Join-Path` 函数，你也可以通过 `String.Concat` 方法来连接多个字符串，或者使用 `StringBuilder` 类来动态构建一个字符串。然而，在 PowerShell 中使用 `+` 运算符是最常见的字符串连接方式，并且也是最简洁明了的方法。

实现细节：
当使用 `+` 运算符连接字符串时，PowerShell 会自动将两个字符串合并成一个新的字符串，并将结果赋值给一个新的变量。当使用 `Join-Path` 函数连接文件路径时，PowerShell 会自动处理不同操作系统之间的路径差异，比如 Windows 和 Linux。

## 相关资源

- Microsoft 文档：[Microsoft 文档-PowerShell 字符串操作](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- YouTube 视频：[PowerShell 字符串连接教程](https://www.youtube.com/watch?v=ZCp1Lm5FQOc)
- GitHub 代码示例集：[PowerShell 字符串操作示例代码](https://github.com/microsoft/PowerShell-Examples)