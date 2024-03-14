---
date: 2024-01-20 17:37:38.219236-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\
  \u3084\u3059\u3044\u30C6\u30AD\u30B9\u30C8\u30D5\u30A9\u30FC\u30E0\u306B\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u53EF\u8AAD\
  \u6027\u3092\u9AD8\u3081\u305F\u308A\u3001\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u304C\u8981\u6C42\u3055\u308C\u308B\u74B0\u5883\u3067\u65E5\u4ED8\u3092\
  \u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.452605-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\
  \u3084\u3059\u3044\u30C6\u30AD\u30B9\u30C8\u30D5\u30A9\u30FC\u30E0\u306B\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u53EF\u8AAD\
  \u6027\u3092\u9AD8\u3081\u305F\u308A\u3001\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u304C\u8981\u6C42\u3055\u308C\u308B\u74B0\u5883\u3067\u65E5\u4ED8\u3092\
  \u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するというのは、日付のデータを読みやすいテキストフォームにすることです。プログラマーは可読性を高めたり、特定のフォーマットが要求される環境で日付を操作するためにこれを行います。

## How to: (実装方法)
日付を文字列に変換する基本的なPowerShellコマンドを見ていきましょう。

```PowerShell
# 現在の日付と時刻を取得
$date = Get-Date

# 標準の形式で文字列に変換
$dateString = $date.ToString()
Write-Host $dateString

# カスタム形式で文字列に変換（例：年-月-日）
$customDateString = $date.ToString("yyyy-MM-dd")
Write-Host $customDateString
```

実行結果かもしれません:
```
2023年3月15日 13:45:30
2023-03-15
```

## Deep Dive (詳細情報)
PowerShellの日付書式指定は.NETに根ざしており、書き方やパターンは.NETのDateTime書式と互換性があります。過去には、人々は特定の日付形式を手作業で文字列に組み込んだものですが、PowerShellの進化により`.ToString()`メソッドを利用することでより簡単にカスタマイズ可能になりました。代わりに、`-f`オペレータや`[datetime]::ParseExact()`メソッドなど、他にも変換オプションがありますが、`.ToString()`は簡単で直感的です。実装の詳細では、ロケールやカルチャ情報（国や言語）が結果に影響することを認識することが重要です。たとえば、アメリカとヨーロッパでは日付の形式が異なります。

## See Also (関連情報)
- [Microsoft's official documentation on custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [PowerShell の Get-Date の使用法](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET の DateTime.ToString メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring?view=net-6.0)
