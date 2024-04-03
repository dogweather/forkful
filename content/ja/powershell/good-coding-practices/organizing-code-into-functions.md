---
date: 2024-01-26 01:11:55.083867-07:00
description: "\u65B9\u6CD5: 2\u3064\u306E\u6570\u306E\u5408\u8A08\u3092\u8A08\u7B97\
  \u3059\u308B\u95A2\u6570\u3092\u66F8\u3044\u3066\u307F\u307E\u3057\u3087\u3046\u3002\
  \u5358\u7D14\u3067\u3059\u304C\u3001\u30DD\u30A4\u30F3\u30C8\u3092\u8AAC\u660E\u3059\
  \u308B\u306B\u306F\u5341\u5206\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.445537-06:00'
model: gpt-4-1106-preview
summary: "2\u3064\u306E\u6570\u306E\u5408\u8A08\u3092\u8A08\u7B97\u3059\u308B\u95A2\
  \u6570\u3092\u66F8\u3044\u3066\u307F\u307E\u3057\u3087\u3046\u3002\u5358\u7D14\u3067\
  \u3059\u304C\u3001\u30DD\u30A4\u30F3\u30C8\u3092\u8AAC\u660E\u3059\u308B\u306B\u306F\
  \u5341\u5206\u3067\u3059."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法:
2つの数の合計を計算する関数を書いてみましょう。単純ですが、ポイントを説明するには十分です。

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# 5と10を使って関数を呼び出す
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "合計は $sum です"
```

サンプル出力:

```
合計は 15 です
```

## 詳しくは:
PowerShellの関数は、他の言語と同様に、古いニュースです。私たちはFortranの日々からコードを区分けしています。それは「車輪を再発明しない」ことについてです。代替手段？確かに、スクリプトやコマンドレットがあります。しかし、スクリプト内の関数のような整頓された感じや文脈に応じた感覚はありません。

実装方法？関数は、私たちの例のような基本的なものから、スコープ、パイプライン入力などを持つ複雑なものまで様々です。`Advanced Functions`を取り上げてみましょう。彼らは、`[Parameter(Mandatory=$true)]`のような属性を持つパラメータを持つコマンドレットを模倣します。それはPowerShellの柔軟性の一端です。

## 参照
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
