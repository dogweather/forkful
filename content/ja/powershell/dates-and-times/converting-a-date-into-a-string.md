---
date: 2024-01-20 17:37:38.219236-07:00
description: "How to: (\u5B9F\u88C5\u65B9\u6CD5) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\
  \u306B\u5909\u63DB\u3059\u308B\u57FA\u672C\u7684\u306APowerShell\u30B3\u30DE\u30F3\
  \u30C9\u3092\u898B\u3066\u3044\u304D\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.452605-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u57FA\
  \u672C\u7684\u306APowerShell\u30B3\u30DE\u30F3\u30C9\u3092\u898B\u3066\u3044\u304D\
  \u307E\u3057\u3087\u3046."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
