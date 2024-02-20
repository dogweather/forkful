---
date: 2024-01-20 17:46:24.358017-07:00
description: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u3068\u306F\
  \uFF1F \u30C6\u30AD\u30B9\u30C8\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u6587\u5B57\
  \u5217\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002 \u306A\u305C\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u306E\u304B\uFF1F\
  \ \u30C7\u30FC\u30BF\u51E6\u7406\u3001\u89E3\u6790\u3001\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u8ABF\u6574\u306B\u4E0D\u53EF\u6B20\u3060\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.541452
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u3068\u306F\
  \uFF1F \u30C6\u30AD\u30B9\u30C8\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u6587\u5B57\
  \u5217\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002 \u306A\u305C\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u306E\u304B\uFF1F\
  \ \u30C7\u30FC\u30BF\u51E6\u7406\u3001\u89E3\u6790\u3001\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u8ABF\u6574\u306B\u4E0D\u53EF\u6B20\u3060\u304B\u3089\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why?
サブストリングの抽出とは？
テキストから特定の部分文字列を取り出すことです。

なぜプログラマーはこれを行うのか？
データ処理、解析、フォーマット調整に不可欠だからです。

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
