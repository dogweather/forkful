---
title:                "数値の丸め処理"
aliases:
- ja/c-sharp/rounding-numbers.md
date:                  2024-01-26T03:44:20.540855-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/rounding-numbers.md"
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
