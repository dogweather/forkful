---
title:                "複素数の扱い方"
date:                  2024-01-26T04:44:21.840862-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、実部と虚部を持つ数（例えば3 + 4iのような）で、工学、物理学、データサイエンスなどの分野で不可欠です。プログラマーは、シミュレーション、信号処理、特定の種類の数学の問題を解決するためにそれらを使用します。

## 方法：
PowerShellには組み込みの複素数サポートがないため、自分でソリューションを作成するか、または.NETの`System.Numerics.Complex`を使用します。

```PowerShell
# .NETを使って複素数を作成しましょう
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# 複素数を作成する
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# 二つの複素数を加算する
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# 二つの複素数を乗算する
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# 結果を表示する
"合計: $sum"
"積: $product"
```
出力：
```
合計: (4, 6)
積: (-5, 10)
```

## 深堀り
複素数は16世紀に実数の範囲内では解が存在しない方程式を解くために開発されました。現代数学の基礎となっています。

PowerShellが.NETの複素数サポートに依存しているため、パフォーマンスは確かです。代替案には、サードパーティのライブラリや、複素数がネイティブなデータ型であるPythonのような他のプログラミング言語が含まれます。

## 参照
- [System.Numerics.Complex 構造体](https://docs.microsoft.com/ja-jp/dotnet/api/system.numerics.complex)
- [Pythonでの複素数算術](https://docs.python.org/3/library/cmath.html)
