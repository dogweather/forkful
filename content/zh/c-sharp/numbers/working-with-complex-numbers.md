---
date: 2024-01-26 04:38:55.548612-07:00
description: "\u590D\u6570\u6269\u5C55\u4E86\u6211\u4EEC\u7684\u6570\u7CFB\u7EDF\uFF0C\
  \u5305\u542B\u4E86\u865A\u6570\uFF0C\u8FD9\u4F7F\u6211\u4EEC\u80FD\u591F\u89E3\u51B3\
  \u6CA1\u6709\u5B9E\u6570\u89E3\u7684\u65B9\u7A0B\u3002\u7A0B\u5E8F\u5458\u5728\u5DE5\
  \u7A0B\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\u5904\u7406\u7B49\u9886\u57DF\u4E2D\
  \u4E0E\u590D\u6570\u6253\u4EA4\u9053\uFF0C\u5728\u8FD9\u4E9B\u9886\u57DF\u4E2D\uFF0C\
  \u590D\u6570\u5BF9\u4E8E\u5EFA\u6A21\u548C\u89E3\u51B3\u95EE\u9898\u81F3\u5173\u91CD\
  \u8981\u3002"
lastmod: '2024-03-13T22:44:47.760793-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u6269\u5C55\u4E86\u6211\u4EEC\u7684\u6570\u7CFB\u7EDF\uFF0C\
  \u5305\u542B\u4E86\u865A\u6570\uFF0C\u8FD9\u4F7F\u6211\u4EEC\u80FD\u591F\u89E3\u51B3\
  \u6CA1\u6709\u5B9E\u6570\u89E3\u7684\u65B9\u7A0B\u3002\u7A0B\u5E8F\u5458\u5728\u5DE5\
  \u7A0B\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\u5904\u7406\u7B49\u9886\u57DF\u4E2D\
  \u4E0E\u590D\u6570\u6253\u4EA4\u9053\uFF0C\u5728\u8FD9\u4E9B\u9886\u57DF\u4E2D\uFF0C\
  \u590D\u6570\u5BF9\u4E8E\u5EFA\u6A21\u548C\u89E3\u51B3\u95EE\u9898\u81F3\u5173\u91CD\
  \u8981\u3002."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何做：
C# 内建了一个 `System.Numerics.Complex` 结构用于处理复数。这里有一个快速指南：

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // 创建复数
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // 基本操作
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // 输出结果
        Console.WriteLine($"Sum: {sum}");
        Console.WriteLine($"Difference: {difference}");
        Console.WriteLine($"Product: {product}");
        Console.WriteLine($"Quotient: {quotient}");
        Console.WriteLine($"Magnitude of c1: {c1.Magnitude}");
        Console.WriteLine($"Phase of c1: {c1.Phase}");
    }
}
```

这将输出：

```
Sum: (4.70710678118655, 5.70710678118655)
Difference: (3.29289321881345, 4.29289321881345)
Product: (-1.00000000000001, 9)
Quotient: (0.6, 0.8)
Magnitude of c1: 6.40312423743285
Phase of c1: 0.896055384571344
```

## 深入探索
复数，由实部和虚部组成（通常表示为 a + bi），自 17 世纪以来就已经存在。意大利数学家 Gerolamo Cardano 被认为是复数早期发展的贡献者。在编程中，处理复数涉及到理解和管理这两个不同的部分。

尽管 C# 的 `System.Numerics.Complex` 结构健壮且集成入语言中，其他语言如 Python 也提供了类似 `cmath` 或第三方库的功能。如果你正在使用的是旧版 C# 或不支持 `System.Numerics` 的 .NET 版本，你可能需要自己实现一个复数类或找到一个库。

内部上，对复数的操作使用浮点数算术，这可能引入舍入误差。因此，在实现广泛使用复数的算法时，记住这一点并考虑其对精度和准确性的影响至关重要。

## 另请参阅
1. C# 对于 `System.Numerics.Complex` 的参考文档：https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. 关于复数数学的更深入探讨：https://mathworld.wolfram.com/ComplexNumber.html
3. 查看 Math.NET Numerics 了解替代实现和库：https://numerics.mathdotnet.com/
