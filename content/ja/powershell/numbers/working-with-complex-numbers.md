---
date: 2024-01-26 04:44:21.840862-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\
  \u8907\u7D20\u6570\u30B5\u30DD\u30FC\u30C8\u304C\u306A\u3044\u305F\u3081\u3001\u81EA\
  \u5206\u3067\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u3092\u4F5C\u6210\u3059\u308B\
  \u304B\u3001\u307E\u305F\u306F.NET\u306E`System.Numerics.Complex`\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.426099-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u8907\u7D20\u6570\u30B5\
  \u30DD\u30FC\u30C8\u304C\u306A\u3044\u305F\u3081\u3001\u81EA\u5206\u3067\u30BD\u30EA\
  \u30E5\u30FC\u30B7\u30E7\u30F3\u3092\u4F5C\u6210\u3059\u308B\u304B\u3001\u307E\u305F\
  \u306F.NET\u306E`System.Numerics.Complex`\u3092\u4F7F\u7528\u3057\u307E\u3059."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

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
