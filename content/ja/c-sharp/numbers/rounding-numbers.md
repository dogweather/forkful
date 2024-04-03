---
date: 2024-01-26 03:44:20.540855-07:00
description: "\u65B9\u6CD5 \u3053\u308C\u304CC\uFF03\u3067\u6570\u5B57\u3092\u4E38\
  \u3081\u308B\u305F\u3081\u306E\u5F80\u5FA9\u30C1\u30B1\u30C3\u30C8\u3067\u3059."
lastmod: '2024-03-13T22:44:42.115934-06:00'
model: gpt-4-0125-preview
summary: "\u3053\u308C\u304CC\uFF03\u3067\u6570\u5B57\u3092\u4E38\u3081\u308B\u305F\
  \u3081\u306E\u5F80\u5FA9\u30C1\u30B1\u30C3\u30C8\u3067\u3059."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法
これがC＃で数字を丸めるための往復チケットです:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // 最も近い整数に丸める
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // 出力：123

        // 小数点以下の桁数を指定する
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // 出力：123.46

        // 次の桁に関わらず切り上げる
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // 出力：124

        // 次の桁に関わらず切り下げる
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // 出力：123
    }
}
```

## 深掘り
昔の話、丸めることは計算コストを削減するための簡単な手段でした。毎サイクルが重要であり、数字を削り落とすことで貴重な時間を節約できました。C# の現代においては、doubleやdecimalの精密さへの偏見や表示の奇妙さを管理することについてです。

`Math.Round`、`Math.Floor`、`Math.Ceiling`を超えて、`MidpointRounding` enumは、私たちに不憫な中間の桁の運命を決定させます。これは、銀行のルールと「半分を切り上げる」公平性の遊び場の間の十字路です。

より厳しい場合、たとえば真剣な数学や金融アプリケーションでは、高精度を提供することで丸めのドラマを減らす`decimal`を`double`に優先します。丸めが少なければ問題も少ない。

## 参考資料
- [`Math.Round`に関する公式C#ドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Double ではなく Decimal をいつ使用すべきか？](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [IEEE浮動小数点算術標準 (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
