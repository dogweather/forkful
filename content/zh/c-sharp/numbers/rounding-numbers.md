---
date: 2024-01-26 03:43:24.578715-07:00
description: "\u5982\u4F55\u505A\uFF1A \u56DE\u5230\u8FC7\u53BB\uFF0C\u56DB\u820D\u4E94\
  \u5165\u662F\u4E3A\u4E86\u51CF\u5C11\u8BA1\u7B97\u6210\u672C\u7684\u7B80\u5355\u4E8B\
  \u60C5\u3002\u6BCF\u4E2A\u5468\u671F\u90FD\u5F88\u91CD\u8981\uFF0C\u51CF\u5C11\u6570\
  \u5B57\u53EF\u4EE5\u8282\u7701\u5B9D\u8D35\u65F6\u95F4\u3002\u5FEB\u8FDB\u5230\u73B0\
  \u4EE3C#\uFF0C\u5B83\u662F\u5173\u4E8E\u7BA1\u7406\u53CC\u7CBE\u5EA6\u548C\u5341\
  \u8FDB\u5236\u7684\u81ED\u540D\u662D\u8457\u7684\u503E\u5411\u4E8E\u7CBE\u5EA6\u9519\
  \u8BEF\u548C\u663E\u793A\u602A\u7656\u3002\u2026"
lastmod: '2024-04-05T22:51:00.970604-06:00'
model: gpt-4-0125-preview
summary: "\u9664\u4E86`Math.Round`\u3001`Math.Floor`\u548C`Math.Ceiling`\u5916\uFF0C\
  `MidpointRounding`\u679A\u4E3E\u8BA9\u6211\u4EEC\u51B3\u5B9A\u5904\u4E8E\u4E2D\u4F4D\
  \u7684\u53EF\u601C\u6570\u5B57\u7684\u547D\u8FD0\u2014\u2014\u5B83\u662F\u94F6\u884C\
  \u89C4\u5219\u548C\u201C\u56DB\u820D\u4E94\u5165\u201D\u516C\u5E73\u6027\u64CD\u573A\
  \u4E4B\u95F4\u7684\u4EA4\u53C9\u70B9\u3002"
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
