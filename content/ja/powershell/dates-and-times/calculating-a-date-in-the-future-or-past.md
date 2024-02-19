---
aliases:
- /ja/powershell/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:37.859345-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u304B\u3089\u65E5\u6570\
  \u3092\u8A08\u7B97\u3057\u3066\u65B0\u305F\u306A\u65E5\u4ED8\u3092\u5C0E\u51FA\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u671F\u9650\u7BA1\u7406\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\
  \u30FC\u30EA\u30F3\u30B0\u3001\u307E\u305F\u306F\u904E\u53BB\u306E\u30C7\u30FC\u30BF\
  \u5206\u6790\u306E\u305F\u3081\u306B\u3053\u306E\u8A08\u7B97\u3092\u884C\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.127002
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u304B\u3089\u65E5\u6570\
  \u3092\u8A08\u7B97\u3057\u3066\u65B0\u305F\u306A\u65E5\u4ED8\u3092\u5C0E\u51FA\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u671F\u9650\u7BA1\u7406\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\
  \u30FC\u30EA\u30F3\u30B0\u3001\u307E\u305F\u306F\u904E\u53BB\u306E\u30C7\u30FC\u30BF\
  \u5206\u6790\u306E\u305F\u3081\u306B\u3053\u306E\u8A08\u7B97\u3092\u884C\u3044\u307E\
  \u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
将来または過去の日付を計算するとは、特定の日から日数を計算して新たな日付を導出することです。プログラマーは、期限管理、イベントのスケジューリング、または過去のデータ分析のためにこの計算を行います。

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
