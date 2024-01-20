---
title:                "检查目录是否存在"
html_title:           "PowerShell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# PowerShell中的目录存在性检查：一个实用的指南

## 何为何解？

在PowerShell编程中，检查目录是否存在即检验系统中是否有指定的文件夹。程序员之所以执行此操作，是为了避免在试图访问不存在的目录时导致的运行错误。

## 操作步骤：

首例，我们通过简单的 `Test-Path` cmdlet 来检查目录是否存在：
```PowerShell
if (Test-Path C:\example) { 
    Write-Host "The directory exists." 
} else { 
    Write-Host "The directory does not exist."
}
```
如果 `C:\example` 这个目录存在，输出将会是 "The directory exists."，否则，将会是 "The directory does not exist."

## 深入探究

`Test-Path` 是PowerShell中用来检查文件或目录是否存在的常用cmdlet，自PowerShell版本1.0以来就存在。然而，也有一些其他的方法可以达到同样的目的，比如 `Get-Item` 和 `Get-ChildItem`，不过它们更适合获取更多关于文件或目录的其他信息。

在实施检查目录存在性的操作时，要明确可读取的路径权限和权限限制。例如，一些特定用户或系统角色可能无权访问某些特定目录，这可能会影响 `Test-Path` 的结果。

## 抛砖引玉

1. PowerShell官方文档，关于 `Test-Path` cmdlet 的介绍：[阅读更多](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)

以上就是PowerShell中检查目录存在性的全部内容，掌握这些可避免许多程序运行错误，实为必备知识。