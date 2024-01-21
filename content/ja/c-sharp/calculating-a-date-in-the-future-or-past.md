---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:31:13.372112-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付計算は、将来または過去の特定の日付を求めることです。予定の管理、期限の設定、履歴データの分析など、現実世界の問題を解決するためにプログラマーが使います。

## How to: (方法)
C#では`DateTime`クラスを使って日付計算をします。数日後や数日前の日付を簡単に求められます。

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        
        // 10日後の日付を計算
        DateTime futureDate = today.AddDays(10);
        Console.WriteLine($"10日後: {futureDate.ToShortDateString()}");

        // 5日前の日付を計算
        DateTime pastDate = today.AddDays(-5);
        Console.WriteLine($"5日前: {pastDate.ToShortDateString()}");
    }
}
```

実行結果 (日付は変わります):

```
10日後: 04/15/2023
5日前: 03/31/2023
```

## Deep Dive (深掘り)
日付計算は、C#が初めて登場した2000年代から利用されています。標準ライブラリーの`System.DateTime`は、日付と時間を表し、操作する機能を提供します。

代替方法として`TimeSpan`を使うこともできますが、日数計算には`DateTime.AddDays`メソッドが便利です。また、タイムゾーンを意識した計算が必要な場合は`DateTimeOffset`を使うことを検討してください。

実装の詳細については、`DateTime`のメソッドはうるう年や月末の日数に対応しているため、独自の計算ロジックを書く必要がありません。ただし、夏時間など特別なケースでは追加の検討が必要です。

## See Also (関連情報)
- [DateTimeクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-6.0)
- [TimeSpanクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.timespan?view=net-6.0)
- [DateTimeOffsetクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetimeoffset?view=net-6.0)