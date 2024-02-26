---
date: 2024-01-26 04:44:21.840862-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\u3092\u6301\
  \u3064\u6570\uFF08\u4F8B\u3048\u30703 + 4i\u306E\u3088\u3046\u306A\uFF09\u3067\u3001\
  \u5DE5\u5B66\u3001\u7269\u7406\u5B66\u3001\u30C7\u30FC\u30BF\u30B5\u30A4\u30A8\u30F3\
  \u30B9\u306A\u3069\u306E\u5206\u91CE\u3067\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u7279\u5B9A\u306E\u7A2E\u985E\u306E\u6570\
  \u5B66\u306E\u554F\u984C\u3092\u89E3\u6C7A\u3059\u308B\u305F\u3081\u306B\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.398988-07:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\u3092\u6301\
  \u3064\u6570\uFF08\u4F8B\u3048\u30703 + 4i\u306E\u3088\u3046\u306A\uFF09\u3067\u3001\
  \u5DE5\u5B66\u3001\u7269\u7406\u5B66\u3001\u30C7\u30FC\u30BF\u30B5\u30A4\u30A8\u30F3\
  \u30B9\u306A\u3069\u306E\u5206\u91CE\u3067\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u7279\u5B9A\u306E\u7A2E\u985E\u306E\u6570\
  \u5B66\u306E\u554F\u984C\u3092\u89E3\u6C7A\u3059\u308B\u305F\u3081\u306B\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
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
