---
date: 2024-01-26 04:44:23.389410-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell \u6CA1\u6709\u5185\u7F6E\u7684\
  \u590D\u6570\u652F\u6301\uFF0C\u6240\u4EE5\u4F60\u8981\u4E48\u81EA\u5DF1\u5B9E\u73B0\
  \u4E00\u4E2A\u89E3\u51B3\u65B9\u6848\uFF0C\u8981\u4E48\u4F7F\u7528 .NET \u7684 `System.Numerics.Complex`\u3002"
lastmod: '2024-03-13T22:44:48.004400-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u6CA1\u6709\u5185\u7F6E\u7684\u590D\u6570\u652F\u6301\uFF0C\u6240\
  \u4EE5\u4F60\u8981\u4E48\u81EA\u5DF1\u5B9E\u73B0\u4E00\u4E2A\u89E3\u51B3\u65B9\u6848\
  \uFF0C\u8981\u4E48\u4F7F\u7528 .NET \u7684 `System.Numerics.Complex`."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作：
PowerShell 没有内置的复数支持，所以你要么自己实现一个解决方案，要么使用 .NET 的 `System.Numerics.Complex`。

```PowerShell
# 使用 .NET 创建复数
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# 创建复数
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# 加两个复数
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# 乘两个复数
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# 显示结果
"Sum: $sum"
"Product: $product"
```
输出：
```
Sum: (4, 6)
Product: (-5, 10)
```

## 深入探讨
复数在 16 世纪被开发出来，用来解决在实数范围内没有解的方程。它们现在是现代数学的基石。

PowerShell 依赖 .NET 来支持复数，意味着性能是稳定的。其他选择包括第三方库或其他编程语言，如 Python，其中复数是原生数据类型。

## 参阅
- [System.Numerics.Complex 结构](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Python 中的复数算术](https://docs.python.org/3/library/cmath.html)
