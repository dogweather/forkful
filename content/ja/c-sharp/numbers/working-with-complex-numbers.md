---
title:                "複素数の扱い方"
aliases:
- /ja/c-sharp/working-with-complex-numbers.md
date:                  2024-01-26T04:38:42.349114-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

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
