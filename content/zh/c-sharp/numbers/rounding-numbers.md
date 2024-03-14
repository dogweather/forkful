---
date: 2024-01-26 03:43:24.578715-07:00
description: "\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u6570\u5B57\u8C03\u6574\
  \u5230\u6700\u63A5\u8FD1\u7684\u6307\u5B9A\u4F4D\u503C\u2014\u2014\u60F3\u8C61\u4E00\
  \u4E0B\u5C06\u5B83\u4EEC\u7B80\u5316\u5230\u4E00\u4E2A\u66F4\u7B80\u5355\u7684\u5F62\
  \u5F0F\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u63A7\u5236\
  \u7CBE\u5EA6\uFF0C\u63D0\u9AD8\u6027\u80FD\uFF0C\u6216\u8005\u5728\u663E\u793A\u7528\
  \u6237\u53CB\u597D\u7684\u7ED3\u679C\u65F6\u2014\u2014\u6BD4\u5982\u4E0D\u9700\u8981\
  \u4E09\u4E2A\u5C0F\u6570\u4F4D\u7684\u4EF7\u683C\u3002"
lastmod: '2024-03-13T22:44:47.761939-06:00'
model: gpt-4-0125-preview
summary: "\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u6570\u5B57\u8C03\u6574\
  \u5230\u6700\u63A5\u8FD1\u7684\u6307\u5B9A\u4F4D\u503C\u2014\u2014\u60F3\u8C61\u4E00\
  \u4E0B\u5C06\u5B83\u4EEC\u7B80\u5316\u5230\u4E00\u4E2A\u66F4\u7B80\u5355\u7684\u5F62\
  \u5F0F\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u4EE5\u63A7\u5236\
  \u7CBE\u5EA6\uFF0C\u63D0\u9AD8\u6027\u80FD\uFF0C\u6216\u8005\u5728\u663E\u793A\u7528\
  \u6237\u53CB\u597D\u7684\u7ED3\u679C\u65F6\u2014\u2014\u6BD4\u5982\u4E0D\u9700\u8981\
  \u4E09\u4E2A\u5C0F\u6570\u4F4D\u7684\u4EF7\u683C\u3002"
title: "\u6570\u5B57\u53D6\u6574"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
四舍五入意味着将数字调整到最接近的指定位值——想象一下将它们简化到一个更简单的形式。程序员进行四舍五入以控制精度，提高性能，或者在显示用户友好的结果时——比如不需要三个小数位的价格。

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
