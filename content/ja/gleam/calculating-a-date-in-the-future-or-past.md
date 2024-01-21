---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:31:12.873530-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？
日付計算って？未来や過去の特定の日にちを算出することだ。プログラマーは何故これをする？予約システムやスケジューリング、有効期限の管理など、日付の計算は必要不可欠だからだ。

## How to:
やり方：
Gleamでの未来や過去の日付の計算例を見てみよう。簡潔に、ポイントを絞って説明する。

```gleam
import gleam/calendar.{ Date, Duration, add_duration }
import gleam/io

fn main() {
  let today = Date(year: 2023, month: 4, day: 14)
  let ten_days = Duration(days: 10)
  let ten_days_later = add_duration(today, ten_days)
  io.println(ten_days_later)
  
  let thirty_days_ago = Duration(days: -30)
  let thirty_days_before = add_duration(today, thirty_days_ago)
  io.println(thirty_days_before)
}
```

サンプル出力：

```gleam
// 10日後の日付
Date(year: 2023, month: 4, day: 24)

// 30日前の日付
Date(year: 2023, month: 3, day: 15)
```

## Deep Dive
深掘り：
日付計算はコンピュータプログラムにおいて長い歴史を持つ。UNIX時間（1970年からの秒数）を基にした方法や、JulianやGregorianカレンダーに従った計算方法もある。

Gleamでは`gleam/calendar`を利用することで、複雑な日付操作が単純化される。`Date`と`Duration`型で直感的に操作できる。

他言語では、JavaScriptには`Date`オブジェクト、Pythonには`datetime`モジュール、Rubyでは`Date`クラスがあり、日付を計算するための異なるアプローチを提供している。

実装の詳細では、閏年の考慮、時差、タイムゾーンなどの複雑な要素が関わってくるが、多くの場合、これらは関連する標準ライブラリによって処理される。

## See Also
参照先：
- Gleamの公式ドキュメント: https://gleam.run/
- `gleam/calendar` モジュール: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- UNIX時間とは: https://ja.wikipedia.org/wiki/UNIX時間
- オンラインでの日付計算ツール： https://www.timeanddate.com/date/dateadd.html