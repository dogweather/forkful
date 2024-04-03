---
date: 2024-01-20 17:48:26.574116-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u7BA1\u7406\u306A\u3069\
  \u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.421637-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u7BA1\u7406\u306A\u3069\
  \u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
