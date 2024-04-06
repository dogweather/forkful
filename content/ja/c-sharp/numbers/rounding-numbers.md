---
date: 2024-01-26 03:44:20.540855-07:00
description: "\u65B9\u6CD5 \u6614\u306E\u8A71\u3001\u4E38\u3081\u308B\u3053\u3068\u306F\
  \u8A08\u7B97\u30B3\u30B9\u30C8\u3092\u524A\u6E1B\u3059\u308B\u305F\u3081\u306E\u7C21\
  \u5358\u306A\u624B\u6BB5\u3067\u3057\u305F\u3002\u6BCE\u30B5\u30A4\u30AF\u30EB\u304C\
  \u91CD\u8981\u3067\u3042\u308A\u3001\u6570\u5B57\u3092\u524A\u308A\u843D\u3068\u3059\
  \u3053\u3068\u3067\u8CB4\u91CD\u306A\u6642\u9593\u3092\u7BC0\u7D04\u3067\u304D\u307E\
  \u3057\u305F\u3002C# \u306E\u73FE\u4EE3\u306B\u304A\u3044\u3066\u306F\u3001double\u3084\
  decimal\u306E\u7CBE\u5BC6\u3055\u3078\u306E\u504F\u898B\u3084\u8868\u793A\u306E\u5947\
  \u5999\u3055\u3092\u7BA1\u7406\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\
  \u3059\u3002\u2026"
lastmod: '2024-04-05T22:50:56.047475-06:00'
model: gpt-4-0125-preview
summary: "`Math.Round`\u3001`Math.Floor`\u3001`Math.Ceiling`\u3092\u8D85\u3048\u3066\
  \u3001`MidpointRounding` enum\u306F\u3001\u79C1\u305F\u3061\u306B\u4E0D\u61AB\u306A\
  \u4E2D\u9593\u306E\u6841\u306E\u904B\u547D\u3092\u6C7A\u5B9A\u3055\u305B\u307E\u3059\
  \u3002\u3053\u308C\u306F\u3001\u9280\u884C\u306E\u30EB\u30FC\u30EB\u3068\u300C\u534A\
  \u5206\u3092\u5207\u308A\u4E0A\u3052\u308B\u300D\u516C\u5E73\u6027\u306E\u904A\u3073\
  \u5834\u306E\u9593\u306E\u5341\u5B57\u8DEF\u3067\u3059\u3002"
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
