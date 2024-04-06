---
date: 2024-01-26 03:43:24.578715-07:00
description: "\u5982\u4F55\u505A\uFF1A \u8FD9\u91CC\u662F\u7528C#\u56DB\u820D\u4E94\
  \u5165\u6570\u5B57\u7684\u5B8C\u6574\u6307\u5357\uFF1A."
lastmod: '2024-04-05T21:53:48.076606-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何做：
这里是用C#四舍五入数字的完整指南：

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // 四舍五入到最近的整数
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // 输出: 123

        // 指定小数位数
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // 输出: 123.46

        // 无论下一位数字为何，始终向上舍入
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // 输出: 124

        // 无论下一位数字为何，始终向下舍入
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // 输出: 123
    }
}
```

## 深入探究
回到过去，四舍五入是为了减少计算成本的简单事情。每个周期都很重要，减少数字可以节省宝贵时间。快进到现代C#，它是关于管理双精度和十进制的臭名昭著的倾向于精度错误和显示怪癖。

除了`Math.Round`、`Math.Floor`和`Math.Ceiling`外，`MidpointRounding`枚举让我们决定处于中位的可怜数字的命运——它是银行规则和“四舍五入”公平性操场之间的交叉点。

对于像是严肃的数学或金融应用这样的困难听众，我们有`decimal`而不是`double`，通过提供更高的精度来减少四舍五入的戏剧性——更少的四舍五入，更少的问题。

## 另请参阅
- [C#官方文档关于`Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: 我应该何时使用Double而不是Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [浮点算术的IEEE标准 (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
