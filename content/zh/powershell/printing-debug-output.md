---
title:                "打印调试输出"
date:                  2024-01-20T17:53:11.996137-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
调试输出是程序运行时信息的展示。程序员用它来追踪代码运作，快速发现和解决bug。

## 如何:
```PowerShell
# 打印简单消息
Write-Host "Hello, Debug!"

# 带条件的调试,在 $debugPreference 设置为 "Continue" 时显示信息
$debugPreference = "Continue"
Debug-Write "Debug mode is on"

# 使用 Write-Debug 打印调试信息 (需要先设置 $DebugPreference)
$DebugPreference = "Continue"
Write-Debug "This is a debug message"

# 输出到警告流
Write-Warning "This is a warning"
```
输出样例：
```
Hello, Debug!
Debug mode is on
DEBUG: This is a debug message
WARNING: This is a warning
```

## 深入了解:
在PowerShell中打印调试信息可以追溯到早期脚本语言，允许程序员理解代码运作。使用`Write-Host`有争议 - 它直接输出到控制台，不适合自动化脚本。更精细的方式是使用`Write-Debug`或`Write-Verbose`，它们提供了开关控制。`Write-Warning`和`Write-Error`则用于更特殊的情况。实现这些命令时，PowerShell使用了不同的输出流来处理不同类型的信息。

## 参见:
- [官方文档: About Write-Debug](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7.2)
- [官方文档: About Write-Verbose](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-verbose?view=powershell-7.2)
- [Stack Overflow: When to use Write-Host, Write-Output, Write-Verbose](https://stackoverflow.com/questions/14048428/powershell-when-to-use-write-host-write-output-write-verbose-etc)
