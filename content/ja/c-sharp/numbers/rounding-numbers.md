---
date: 2024-01-26 03:44:20.540855-07:00
description: "\u6570\u5B57\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6307\u5B9A\u3055\
  \u308C\u305F\u6841\u5024\u306B\u6700\u3082\u8FD1\u3044\u5F62\u306B\u8ABF\u6574\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3064\u307E\u308A\u3001\
  \u305D\u308C\u3092\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u306A\u5F62\u306B\u56FA\u5B9A\
  \u3059\u308B\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u7CBE\u5EA6\u3092\u5236\u5FA1\u3057\u305F\u308A\u3001\u30D1\
  \u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u5411\u4E0A\u3055\u305B\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u7D50\u679C\u3092\
  \u8868\u793A\u3059\u308B\u969B\uFF08\uFF13\u3064\u306E\u5C0F\u6570\u70B9\u3092\u5FC5\
  \u8981\u3068\u3057\u306A\u3044\u4FA1\u683C\u306A\u3069\uFF09\u306B\u4E38\u3081\u307E\
  \u3059\u3002"
lastmod: '2024-02-25T18:49:40.134259-07:00'
model: gpt-4-0125-preview
summary: "\u6570\u5B57\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6307\u5B9A\u3055\
  \u308C\u305F\u6841\u5024\u306B\u6700\u3082\u8FD1\u3044\u5F62\u306B\u8ABF\u6574\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3064\u307E\u308A\u3001\
  \u305D\u308C\u3092\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u306A\u5F62\u306B\u56FA\u5B9A\
  \u3059\u308B\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u7CBE\u5EA6\u3092\u5236\u5FA1\u3057\u305F\u308A\u3001\u30D1\
  \u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u5411\u4E0A\u3055\u305B\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u7D50\u679C\u3092\
  \u8868\u793A\u3059\u308B\u969B\uFF08\uFF13\u3064\u306E\u5C0F\u6570\u70B9\u3092\u5FC5\
  \u8981\u3068\u3057\u306A\u3044\u4FA1\u683C\u306A\u3069\uFF09\u306B\u4E38\u3081\u307E\
  \u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となく理由
数字を丸めるとは、指定された桁値に最も近い形に調整することを意味します。つまり、それをよりシンプルな形に固定するようなものです。プログラマーは、精度を制御したり、パフォーマンスを向上させたり、ユーザーフレンドリーな結果を表示する際（３つの小数点を必要としない価格など）に丸めます。

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
