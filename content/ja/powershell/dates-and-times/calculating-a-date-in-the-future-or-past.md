---
date: 2024-01-20 17:31:37.859345-07:00
description: "How to: (\u65B9\u6CD5) \u6B74\u53F2\u7684\u306A\u80CC\u666F\u304B\u3089\
  \u898B\u308B\u3068\u3001\u65E5\u4ED8\u8A08\u7B97\u306F\u7D19\u306E\u30AB\u30EC\u30F3\
  \u30C0\u30FC\u306B\u3088\u3063\u3066\u624B\u4F5C\u696D\u3067\u884C\u308F\u308C\u3066\
  \u3044\u307E\u3057\u305F\u304C\u3001\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30FC\u306E\
  \u767B\u5834\u306B\u3088\u308A\u3001\u305D\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u305A\
  \u3063\u3068\u901F\u304F\u6B63\u78BA\u306B\u306A\u308A\u307E\u3057\u305F\u3002PowerShell\u3067\
  \u306F\u3001`Get-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.275637-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6B74\u53F2\u7684\u306A\u80CC\u666F\u304B\u3089\u898B\u308B\
  \u3068\u3001\u65E5\u4ED8\u8A08\u7B97\u306F\u7D19\u306E\u30AB\u30EC\u30F3\u30C0\u30FC\
  \u306B\u3088\u3063\u3066\u624B\u4F5C\u696D\u3067\u884C\u308F\u308C\u3066\u3044\u307E\
  \u3057\u305F\u304C\u3001\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30FC\u306E\u767B\u5834\
  \u306B\u3088\u308A\u3001\u305D\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u305A\u3063\u3068\
  \u901F\u304F\u6B63\u78BA\u306B\u306A\u308A\u307E\u3057\u305F\u3002PowerShell\u3067\
  \u306F\u3001`Get-Date`\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u7528\
  \u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3057\
  \u3001`.Add...`\u30E1\u30BD\u30C3\u30C9\uFF08\u4F8B:`AddDays`, `AddHours`, `AddMonths`\
  \ \u306A\u3069\uFF09\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u52A0\u7B97\u307E\
  \u305F\u306F\u6E1B\u7B97\u3057\u307E\u3059\u3002\u4ED6\u306E\u4EE3\u66FF\u65B9\u6CD5\
  \u3068\u3057\u3066\u306F\u3001`.NET`\u306EDateTime\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u3092\u76F4\u63A5\u64CD\u4F5C\u3059\u308B\u3053\u3068\u3082\u53EF\u80FD\u3067\
  \u3059\u304C\u3001PowerShell\u306E\u7D44\u307F\u8FBC\u307F\u6A5F\u80FD\u3092\u4F7F\
  \u7528\u3059\u308B\u65B9\u304C\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002\u52A0\u7B97\
  /\u6E1B\u7B97\u3055\u308C\u305F\u65E5\u4ED8\u306E\u9069\u7528\u7BC4\u56F2\u306F\u3001\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u5185\u3067\u306E\u30BF\u30A4\u30DF\u30F3\u30B0\u64CD\
  \u4F5C\u3001\u30ED\u30B0\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3001\u30E6\
  \u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30D5\u30A7\u30FC\u30B9\u306E\u65E5\u4ED8\u30D4\
  \u30C3\u30AB\u30FC\u306A\u3069\u5E83\u7BC4\u56F2\u306B\u53CA\u3073\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (方法)
```PowerShell
# 5日後の日付を取得する
$futureDate = (Get-Date).AddDays(5)
Write-Output $futureDate

# 出力例: Thursday, March 31, 2023 10:46:22 AM

# 10日前の日付を取得する
$pastDate = (Get-Date).AddDays(-10)
Write-Output $pastDate

# 出力例: Sunday, March 16, 2023 10:46:22 AM
```

## Deep Dive (詳細な説明)
歴史的な背景から見ると、日付計算は紙のカレンダーによって手作業で行われていましたが、コンピューターの登場により、そのプロセスはずっと速く正確になりました。PowerShellでは、`Get-Date`コマンドレットを使用して現在の日付と時刻を取得し、`.Add...`メソッド（例:`AddDays`, `AddHours`, `AddMonths` など）を使って日付を加算または減算します。他の代替方法としては、`.NET`のDateTimeオブジェクトを直接操作することも可能ですが、PowerShellの組み込み機能を使用する方がシンプルです。加算/減算された日付の適用範囲は、スクリプト内でのタイミング操作、ログのタイムスタンプ、ユーザーインタフェースの日付ピッカーなど広範囲に及びます。

## See Also (関連情報)
- PowerShellの公式ドキュメント: [Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- オンライン時間と日付の計算ツール: [timeanddate.com](https://www.timeanddate.com/date/dateadd.html)
- `.NET`のDateTime構造について: [DateTime Struct](https://docs.microsoft.com/dotnet/api/system.datetime?view=net-6.0)
