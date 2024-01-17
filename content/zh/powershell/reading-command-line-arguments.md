---
title:                "读取命令行参数"
html_title:           "PowerShell: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么是读取命令行参数？为什么程序员需要这样做？
读取命令行参数是指通过命令行输入的信息，供程序使用。程序员经常使用这种方法来获取用户输入的信息或配置参数，从而定制程序的行为。

## 如何读取命令行参数?
使用PowerShell内置的$argv变量，可以很容易地访问用户通过命令行输入的参数。下面是一个简单的示例代码和输出结果，展示了如何读取命令行参数。

```PowerShell
Set-Location C:\Users\Username\Desktop
# 假设运行脚本时，使用命令： PowerShell script.ps1 Parameter1 Parameter2
Write-Host "第一个参数： $args[0]"
Write-Host "第二个参数： $args[1]"
```

输出：

```
第一个参数： Parameter1
第二个参数： Parameter2
```

## 深入了解
### 历史背景
命令行参数最初是由Unix操作系统引入的概念，用于向程序传递参数。它被广泛应用于操作系统的命令行界面和脚本语言中，如Bash和PowerShell。
### 替代方法
除了读取命令行参数外，程序员还可以通过其他方式来获取用户输入的信息，例如使用图形界面或配置文件。但是，读取命令行参数仍然被认为是一种简单且有效的方法。
### 实现细节
在PowerShell中，使用$argv变量来访问命令行参数，它将所有参数存储在一个数组中。$args[0]代表第一个参数，以此类推。如果命令行没有提供任何参数，$args数组将为空。

## 相关资源
- Microsoft 的官方 PowerShell 文档：https://docs.microsoft.com/en-us/powershell
- 从命令行读取参数的其他方法：https://unix.stackexchange.com/questions/31414/how-can-i-pass-a-command-line-argument-into-a-shell-script
- PowerShell 脚本编程教程：https://www.tutorialspoint.com/powershell/