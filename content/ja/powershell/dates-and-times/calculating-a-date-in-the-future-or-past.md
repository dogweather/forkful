---
date: 2024-01-20 17:31:37.859345-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.456090-06:00'
model: gpt-4-1106-preview
summary: .
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
