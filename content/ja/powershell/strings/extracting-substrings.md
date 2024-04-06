---
date: 2024-01-20 17:46:24.358017-07:00
description: "How to: \u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u306F\u3001\
  \u6587\u5B57\u5217\u64CD\u4F5C\u306E\u57FA\u672C\u3067\u3059\u3002`Substring`\u30E1\
  \u30BD\u30C3\u30C9\u306F.NET Framework\u306E\u767B\u5834\u4EE5\u6765\u3001\u4E00\
  \u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u5206\u5272\
  (`Split`)\u3084\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u3063\u305F\u62BD\u51FA\u3082\
  \u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.244192-06:00'
model: gpt-4-1106-preview
summary: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u306F\u3001\u6587\u5B57\
  \u5217\u64CD\u4F5C\u306E\u57FA\u672C\u3067\u3059\u3002`Substring`\u30E1\u30BD\u30C3\
  \u30C9\u306F.NET Framework\u306E\u767B\u5834\u4EE5\u6765\u3001\u4E00\u822C\u7684\
  \u306B\u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u5206\u5272(`Split`)\u3084\
  \u6B63\u898F\u8868\u73FE\u3092\u4F7F\u3063\u305F\u62BD\u51FA\u3082\u3088\u304F\u4F7F\
  \u308F\u308C\u307E\u3059."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to:
```PowerShell
# 文字列の宣言
$string = "PowerShellは楽しいです！"

# 部分文字列の抽出 - 開始位置から文字数を指定
$substring = $string.Substring(0, 10)
$substring  # 出力: PowerShell

# 特定の文字で分割して部分文字列を取得
$splitString = $string.Split("は")[1]
$splitString  # 出力: 楽しいです！

# 正規表現を使用してマッチする部分文字列を抽出
$matchedString = $string -match "楽しい"
$matches[0]  # 出力: 楽しい
```

## Deep Dive
部分文字列の抽出は、文字列操作の基本です。`Substring`メソッドは.NET Frameworkの登場以来、一般的に使用されています。分割(`Split`)や正規表現を使った抽出もよく使われます。

歴史的に、文字列の操作はプログラミングにおいて常に重要な役割を果たしてきました。初期のプログラミング言語から、現在の高度なスクリプト言語に至るまで、文字列処理の機能は進化し続けています。

また、`-match` 演算子や `[regex]::Match` といった正規表現を用いた方法は、検索パターンが複雑な場合に強力です。`-match` 演算子はPowerShellに組み込まれた`$matches`自動変数にマッチした結果を格納する特性があります。

## See Also
- PowerShellにおける文字列の操作方法: [about_Split](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_split)
- PowerShellでの正規表現: [about_Regular_Expressions](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
