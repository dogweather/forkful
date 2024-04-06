---
date: 2024-01-20 17:51:27.944971-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1A \u5B57\u7B26\u4E32\u63D2\u503C\
  \u5728 PowerShell \u4E2D\u76F8\u5F53\u76F4\u63A5\u3002\u5728\u53CC\u5F15\u53F7\u5185\
  \uFF0C\u901A\u8FC7 `$variableName` \u6765\u63D2\u503C\u53D8\u91CF\u3002PowerShell\
  \ 5.0 \u5F15\u5165\u4E86 `$\"{}\"`\uFF0C\u7ED9\u590D\u6742\u8868\u8FBE\u5F0F\u63D0\
  \u4F9B\u4E86\u989D\u5916\u7684\u7A7A\u95F4\u3002 \u5728\u6B64\u4E4B\u524D\uFF0C\u7A0B\
  \u5E8F\u5458\u5F97\u7528\u62FC\u63A5\u7684\u65B9\u5F0F\uFF0C\u6BD4\u5982 `$\"Hello,\
  \ \" + $name +\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.151349-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5B57\u7B26\u4E32\u63D2\u503C\u5728 PowerShell\
  \ \u4E2D\u76F8\u5F53\u76F4\u63A5\u3002\u5728\u53CC\u5F15\u53F7\u5185\uFF0C\u901A\
  \u8FC7 `$variableName` \u6765\u63D2\u503C\u53D8\u91CF\u3002PowerShell 5.0 \u5F15\
  \u5165\u4E86 `$\"{}\"`\uFF0C\u7ED9\u590D\u6742\u8868\u8FBE\u5F0F\u63D0\u4F9B\u4E86\
  \u989D\u5916\u7684\u7A7A\u95F4\u3002"
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
