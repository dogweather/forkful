---
date: 2024-01-26 01:11:55.083867-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.445537-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\
  \u3059\u308B\u30B3\u30FC\u30C9\u306E\u584A\u3092\u30E9\u30C3\u30D7\u3057\u3001\u305D\
  \u308C\u306B\u540D\u524D\u3092\u4ED8\u3051\u308B\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u30B3\u30FC\u30C9\u3092\u518D\u5229\u7528\u53EF\u80FD\u3001\u8AAD\
  \u307F\u3084\u3059\u304F\u3001\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\
  \u3081\u306B\u884C\u308F\u308C\u307E\u3059\u3002\u540C\u3058\u30B3\u30FC\u30C9\u3092\
  \u66F8\u304D\u76F4\u3059\u4EE3\u308F\u308A\u306B\u3001\u95A2\u6570\u3092\u547C\u3073\
  \u51FA\u3057\u307E\u3059\u3002\u30C8\u30E9\u30D6\u30EB\u30B7\u30E5\u30FC\u30C6\u30A3\
  \u30F3\u30B0\u3084\u30A2\u30C3\u30D7\u30B0\u30EC\u30FC\u30C9\u3092\u3057\u305F\u3044\
  \u3067\u3059\u304B\uFF1F\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u5C71\u3092\u304B\u304D\
  \u5206\u3051\u308B\u3053\u3068\u306A\u304F\u3001\u95A2\u6570\u3092\u5FAE\u8ABF\u6574\
  \u3057\u307E\u3059\u3002."
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
