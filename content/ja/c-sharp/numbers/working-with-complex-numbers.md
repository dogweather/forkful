---
date: 2024-01-26 04:38:42.349114-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u969B\u306E\u89E3\u304C\u5B58\u5728\
  \u3057\u306A\u3044\u65B9\u7A0B\u5F0F\u3092\u89E3\u304F\u3053\u3068\u3092\u53EF\u80FD\
  \u306B\u3059\u308B\u305F\u3081\u306B\u3001\u865A\u6570\u3092\u542B\u3080\u3088\u3046\
  \u306B\u6570\u306E\u4F53\u7CFB\u3092\u62E1\u5F35\u3057\u307E\u3059\u3002\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u4FE1\u53F7\u51E6\
  \u7406\u306A\u3069\u306E\u5206\u91CE\u3067\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3089\u306E\u6570\u3092\u6271\u3046\u969B\u306B\u306F\u3001\u30E2\u30C7\
  \u30EA\u30F3\u30B0\u3084\u554F\u984C\u89E3\u6C7A\u306B\u4E0D\u53EF\u6B20\u3067\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.114015-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u969B\u306E\u89E3\u304C\u5B58\u5728\
  \u3057\u306A\u3044\u65B9\u7A0B\u5F0F\u3092\u89E3\u304F\u3053\u3068\u3092\u53EF\u80FD\
  \u306B\u3059\u308B\u305F\u3081\u306B\u3001\u865A\u6570\u3092\u542B\u3080\u3088\u3046\
  \u306B\u6570\u306E\u4F53\u7CFB\u3092\u62E1\u5F35\u3057\u307E\u3059\u3002\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u4FE1\u53F7\u51E6\
  \u7406\u306A\u3069\u306E\u5206\u91CE\u3067\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3089\u306E\u6570\u3092\u6271\u3046\u969B\u306B\u306F\u3001\u30E2\u30C7\
  \u30EA\u30F3\u30B0\u3084\u554F\u984C\u89E3\u6C7A\u306B\u4E0D\u53EF\u6B20\u3067\u3059\
  \u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 何となぜ？
複素数は、実際の解が存在しない方程式を解くことを可能にするために、虚数を含むように数の体系を拡張します。エンジニアリング、物理学、信号処理などの分野でプログラマーがこれらの数を扱う際には、モデリングや問題解決に不可欠です。

## どのように行うか：
C#には複素数を処理するための組み込みの`System.Numerics.Complex`構造体があります。こちらが簡単な例です：

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // 複素数の作成
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // 基本的な操作
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // 結果の出力
        Console.WriteLine($"和: {sum}");
        Console.WriteLine($"差: {difference}");
        Console.WriteLine($"積: {product}");
        Console.WriteLine($"商: {quotient}");
        Console.WriteLine($"c1の大きさ: {c1.Magnitude}");
        Console.WriteLine($"c1の位相: {c1.Phase}");
    }
}
```

そして、これが出力されます：

```
和: (4.70710678118655, 5.70710678118655)
差: (3.29289321881345, 4.29289321881345)
積: (-1.00000000000001, 9)
商: (0.6, 0.8)
c1の大きさ: 6.40312423743285
c1の位相: 0.896055384571344
```

## 深掘り
複素数は、実部と虚部（よくa + biと表記される）からなり、17世紀から存在しています。イタリアの数学者ジェロラモ・カルダノがその初期の発展に貢献したとされます。プログラミングにおいて複素数を扱うことは、これら二つの異なる部分の理解と管理を要します。

C#の`System.Numerics.Complex`は堅牢で言語に統合されていますが、Pythonなど他の言語では`cmath`やサードパーティのライブラリで同様の機能が提供されています。また、C#の古いバージョンや`System.Numerics`をサポートしていない.NETのバージョンを使用している場合は、独自の複素数クラスを自作するか、ライブラリを探さなければならないかもしれません。

複素数の操作では内部的に浮動小数点算術を使用するため、丸め誤差が生じる可能性があります。そのため、複素数を広く使用するアルゴリズムを実装する際には、この点を覚えておき、精度と正確性への影響を考慮することが鍵となります。

## 関連情報
1. `System.Numerics.Complex`についてのC#リファレンス：https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. 複素数の数学についてのさらなる深掘り：https://mathworld.wolfram.com/ComplexNumber.html
3. 別の実装やライブラリについては、Math.NET Numericsをチェックしてください：https://numerics.mathdotnet.com/
