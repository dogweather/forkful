---
date: 2024-01-20 17:53:11.996137-07:00
description: "\u5982\u4F55: \u5728PowerShell\u4E2D\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\
  \u53EF\u4EE5\u8FFD\u6EAF\u5230\u65E9\u671F\u811A\u672C\u8BED\u8A00\uFF0C\u5141\u8BB8\
  \u7A0B\u5E8F\u5458\u7406\u89E3\u4EE3\u7801\u8FD0\u4F5C\u3002\u4F7F\u7528`Write-Host`\u6709\
  \u4E89\u8BAE - \u5B83\u76F4\u63A5\u8F93\u51FA\u5230\u63A7\u5236\u53F0\uFF0C\u4E0D\
  \u9002\u5408\u81EA\u52A8\u5316\u811A\u672C\u3002\u66F4\u7CBE\u7EC6\u7684\u65B9\u5F0F\
  \u662F\u4F7F\u7528`Write-Debug`\u6216`Write-Verbose`\uFF0C\u5B83\u4EEC\u63D0\u4F9B\
  \u4E86\u5F00\u5173\u63A7\u5236\u3002`Write-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.313385-06:00'
model: gpt-4-1106-preview
summary: "\u5728PowerShell\u4E2D\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u53EF\u4EE5\u8FFD\
  \u6EAF\u5230\u65E9\u671F\u811A\u672C\u8BED\u8A00\uFF0C\u5141\u8BB8\u7A0B\u5E8F\u5458\
  \u7406\u89E3\u4EE3\u7801\u8FD0\u4F5C\u3002\u4F7F\u7528`Write-Host`\u6709\u4E89\u8BAE\
  \ - \u5B83\u76F4\u63A5\u8F93\u51FA\u5230\u63A7\u5236\u53F0\uFF0C\u4E0D\u9002\u5408\
  \u81EA\u52A8\u5316\u811A\u672C\u3002\u66F4\u7CBE\u7EC6\u7684\u65B9\u5F0F\u662F\u4F7F\
  \u7528`Write-Debug`\u6216`Write-Verbose`\uFF0C\u5B83\u4EEC\u63D0\u4F9B\u4E86\u5F00\
  \u5173\u63A7\u5236\u3002`Write-Warning`\u548C`Write-Error`\u5219\u7528\u4E8E\u66F4\
  \u7279\u6B8A\u7684\u60C5\u51B5\u3002\u5B9E\u73B0\u8FD9\u4E9B\u547D\u4EE4\u65F6\uFF0C\
  PowerShell\u4F7F\u7528\u4E86\u4E0D\u540C\u7684\u8F93\u51FA\u6D41\u6765\u5904\u7406\
  \u4E0D\u540C\u7C7B\u578B\u7684\u4FE1\u606F\u3002"
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
