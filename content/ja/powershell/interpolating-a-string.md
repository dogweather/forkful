---
title:                "文字列の補間"
date:                  2024-01-20T17:51:22.103093-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間は、変数や式の結果を文字列に直接埋め込むことです。これを使う理由は簡単で、コードを読みやすくし、動的な内容を生成するためです。

## How to (やり方):
```PowerShell
# 文字列補間の基本
$name = "世界"
$greeting = "こんにちは、$name！"
Write-Output $greeting  # こんにちは、世界！

# 式を含む補間
$price = 1000
$taxRate = 0.08
$total = "合計：$($price * (1 + $taxRate))円"
Write-Output $total  # 合計：1080円
```

## Deep Dive (詳細情報):
文字列補間は、PowerShellに限らず多くのプログラミング言語で使われています。始まりはPerlやRubyのような言語で、これらは動的な文字列操作を重要視してきました。PowerShellではダブルクオート(")の中に変数や式を$記号で始めることで、簡単に文字列に埋め込むことができます。

以前のPowerShellのバージョンや他のシェルスクリプト言語では、文字列の連結を使う必要がありました。たとえば:

```PowerShell
$name = "世界"
$greeting = "こんにちは、" + $name + "！"
```

これは機能しますが、文字列補間の方が直感的でエラーも少ないです。

実装の面では、文字列補間はPowerShellの言語エンジンによって行われ、実行時に変数や式が評価されて文字列に埋め込まれます。これにより、プログラムの可読性とメンテナンス性が向上します。

## See Also (参考資料):
- [About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
- [About Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators)
