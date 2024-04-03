---
date: 2024-01-20 17:53:11.996137-07:00
description: "\u8C03\u8BD5\u8F93\u51FA\u662F\u7A0B\u5E8F\u8FD0\u884C\u65F6\u4FE1\u606F\
  \u7684\u5C55\u793A\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u8FFD\u8E2A\u4EE3\u7801\
  \u8FD0\u4F5C\uFF0C\u5FEB\u901F\u53D1\u73B0\u548C\u89E3\u51B3bug\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.016201-06:00'
model: gpt-4-1106-preview
summary: "\u8C03\u8BD5\u8F93\u51FA\u662F\u7A0B\u5E8F\u8FD0\u884C\u65F6\u4FE1\u606F\
  \u7684\u5C55\u793A\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u8FFD\u8E2A\u4EE3\u7801\
  \u8FD0\u4F5C\uFF0C\u5FEB\u901F\u53D1\u73B0\u548C\u89E3\u51B3bug\u3002."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
