---
date: 2024-01-20 17:48:26.574116-07:00
description: "How to: (\u65B9\u6CD5) PowerShell\u3067\u6587\u5B57\u5217\u306E\u9577\
  \u3055\u3092\u898B\u3064\u3051\u308B\u306B\u306F\u3001`.Length`\u30D7\u30ED\u30D1\
  \u30C6\u30A3\u3092\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.246801-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PowerShell\u3067\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\
  \u898B\u3064\u3051\u308B\u306B\u306F\u3001`.Length`\u30D7\u30ED\u30D1\u30C6\u30A3\
  \u3092\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to: (方法)
PowerShellで文字列の長さを見つけるには、`.Length`プロパティを使います。

```PowerShell
$text = "こんにちは、世界！"
$textLength = $text.Length
$textLength
```

サンプル出力：

```
9
```

この例では、`$text`の文字列の長さが`9`と表示されます。これには日本語の文字も含まれています。

## Deep Dive (詳細な情報)
PowerShellで`.Length`プロパティを使うと、.NETの`System.String`クラスを介して文字列の長さを求めます。歴史的には、多くのプログラミング言語は長さや他の文字列操作をサポートする独自の機能を持ちます。`.Length`はPowerShellにおける直感的で簡単な方法の一つですが、他にも`$text.ToCharArray().Count`や`$text | Measure-Object -Character`のような方法があります。

文字列の長さを見つける時の実装詳細：
- Unicode: PowerShellはUnicodeをサポートしており、全ての文字を適切に数えます。
- 文字列の終端: PowerShellでの文字列は終端ナル文字を必要としません。

これらの違いは異なる環境や要求に適応するために重要です。

## See Also (関連リンク)
- [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1) - PowerShellの自動変数に関する公式ドキュメント。
- [about_Properties](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_properties?view=powershell-7.1) - PowerShellのプロパティに関する公式ドキュメント。
