---
title:                "「将来または過去の日付の計算」"
html_title:           "Gleam: 「将来または過去の日付の計算」"
simple_title:         "「将来または過去の日付の計算」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

＃＃ なに？なぜ？
未来や過去の日付を計算するとは？なぜプログラマーがそれを行うのかを説明します。

＃＃ 方法：
以下のコード例とサンプル出力を使用して、計算方法を説明します。

```Gleam
// 今日の日付を取得
let today = Date.now()

// 1日後の日付を計算
let future = Date.add_days(today, 1)

// 結果を出力
IO.inspect(future) // 出力例: 2021-07-01T00:00:00Z
```

```Gleam
// 今日の日付を取得
let today = Date.now()

// 1ヶ月前の日付を計算
let past = Date.add_months(today, -1)

// 結果を出力
IO.inspect(past) // 出力例: 2021-05-31T00:00:00Z
```

＃＃ ディープダイブ：
未来や過去の日付を計算する歴史的な背景、代替手段、および実装詳細について説明します。

＃＃ 関連リンク：
関連情報へのリンクを紹介します。