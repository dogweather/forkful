---
date: 2024-01-20 17:51:22.103093-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306B\u76F4\u63A5\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u3053\u308C\u3092\u4F7F\u3046\u7406\u7531\u306F\u7C21\u5358\
  \u3067\u3001\u30B3\u30FC\u30C9\u3092\u8AAD\u307F\u3084\u3059\u304F\u3057\u3001\u52D5\
  \u7684\u306A\u5185\u5BB9\u3092\u751F\u6210\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.413558-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306B\u76F4\u63A5\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u3053\u308C\u3092\u4F7F\u3046\u7406\u7531\u306F\u7C21\u5358\
  \u3067\u3001\u30B3\u30FC\u30C9\u3092\u8AAD\u307F\u3084\u3059\u304F\u3057\u3001\u52D5\
  \u7684\u306A\u5185\u5BB9\u3092\u751F\u6210\u3059\u308B\u305F\u3081\u3067\u3059\u3002\
  ."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
