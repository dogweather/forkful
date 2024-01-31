---
title:                "字符串插值"
date:                  2024-01-20T17:51:27.944971-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

category:             "PowerShell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
字符串插值是把变量或表达式的值嵌入到字符串中的过程。程序员这么做是为了动态构建字符串和提升代码的可读性。

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
