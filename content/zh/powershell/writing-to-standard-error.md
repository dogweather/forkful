---
title:                "写入标准错误"
html_title:           "PowerShell: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误及其用途？
写入标准错误是指将程序执行中的错误信息输出到屏幕的特定位置，它与标准输出不同，后者主要用于输出程序处理结果。程序员通常会使用写入标准错误的方式来帮助排查程序中的错误。

## 如何实现：
```PowerShell
Write-Error "这是一个标准错误信息" 
```
输出：
```
这是一个标准错误信息
```

## 深入了解：
写入标准错误的概念最早出现在Unix操作系统中，随后被多种编程语言所采用，包括PowerShell。除了使用Write-Error命令之外，程序员也可以使用Write-Host命令将信息输出到屏幕。不过，Write-Host输出的信息不会被记录到日志文件中，而Write-Error则可以被记录下来，方便程序员查看错误信息并进行调试。

## 更多资源：
- [PowerShell官方文档](https://docs.microsoft.com/zh-cn/powershell/scripting/core-powershell/console/redirecting-standard-error?view=powershell-7)
- [博客：学习PowerShell基础知识](https://khylo.github.io/2019/01/17/powershell-the-basics/)