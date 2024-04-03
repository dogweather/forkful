---
date: 2024-01-26 04:44:23.389410-07:00
description: "\u590D\u6570\u662F\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u6570\
  \uFF08\u5982 3 + 4i\uFF09\uFF0C\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\
  \u6570\u636E\u79D1\u5B66\u7B49\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u5B83\u4EEC\u8FDB\u884C\u6A21\u62DF\u3001\u4FE1\u53F7\u5904\u7406\
  \u548C\u89E3\u51B3\u7279\u5B9A\u7C7B\u578B\u7684\u6570\u5B66\u95EE\u9898\u3002"
lastmod: '2024-03-13T22:44:48.004400-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u662F\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u6570\
  \uFF08\u5982 3 + 4i\uFF09\uFF0C\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\
  \u6570\u636E\u79D1\u5B66\u7B49\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u5B83\u4EEC\u8FDB\u884C\u6A21\u62DF\u3001\u4FE1\u53F7\u5904\u7406\
  \u548C\u89E3\u51B3\u7279\u5B9A\u7C7B\u578B\u7684\u6570\u5B66\u95EE\u9898\u3002."
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
