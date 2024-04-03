---
date: 2024-01-20 17:51:27.944971-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u628A\u53D8\u91CF\u6216\u8868\u8FBE\
  \u5F0F\u7684\u503C\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\
  \u7B26\u4E32\u548C\u63D0\u5347\u4EE3\u7801\u7684\u53EF\u8BFB\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.995578-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u628A\u53D8\u91CF\u6216\u8868\u8FBE\
  \u5F0F\u7684\u503C\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\
  \u7B26\u4E32\u548C\u63D0\u5347\u4EE3\u7801\u7684\u53EF\u8BFB\u6027\u3002."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## How to: 如何操作：
```PowerShell
# Define a variable
$name = '世界'

# Interpolate the variable into a string
$greeting = "你好, $name！"

# Print the result
Write-Output $greeting
```
输出: 
```
你好, 世界！
```

## Deep Dive 深入探讨
字符串插值在 PowerShell 中相当直接。在双引号内，通过 `$variableName` 来插值变量。PowerShell 5.0 引入了 `$"{}"`，给复杂表达式提供了额外的空间。

在此之前，程序员得用拼接的方式，比如 `$"Hello, " + $name + "!"`。这种方法不太直观，难以阅读，特别是当拼接的字符串很长或很复杂时。

在 PowerShell 中，你可以插入任何类型的变量，不只是字符串。如果你插入的不是字符串，PowerShell 会自动调用 `.ToString()` 方法转换成字符串。

## See Also 相关链接
- [About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
