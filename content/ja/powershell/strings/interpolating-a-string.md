---
date: 2024-01-20 17:51:22.103093-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.241121-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001PowerShell\u306B\u9650\u3089\u305A\
  \u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u4F7F\
  \u308F\u308C\u3066\u3044\u307E\u3059\u3002\u59CB\u307E\u308A\u306FPerl\u3084Ruby\u306E\
  \u3088\u3046\u306A\u8A00\u8A9E\u3067\u3001\u3053\u308C\u3089\u306F\u52D5\u7684\u306A\
  \u6587\u5B57\u5217\u64CD\u4F5C\u3092\u91CD\u8981\u8996\u3057\u3066\u304D\u307E\u3057\
  \u305F\u3002PowerShell\u3067\u306F\u30C0\u30D6\u30EB\u30AF\u30AA\u30FC\u30C8(\"\
  )\u306E\u4E2D\u306B\u5909\u6570\u3084\u5F0F\u3092$\u8A18\u53F7\u3067\u59CB\u3081\
  \u308B\u3053\u3068\u3067\u3001\u7C21\u5358\u306B\u6587\u5B57\u5217\u306B\u57CB\u3081\
  \u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
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
