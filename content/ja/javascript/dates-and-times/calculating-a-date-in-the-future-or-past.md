---
title:                "将来または過去の日付を計算する"
aliases: - /ja/javascript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:33.014965-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付の計算は、現在から見て未来や過去の特定の日を求めることです。タイムライン上でイベント予定を設定するためや、期限を計算するためにプログラマーはこの計算を行います。

## How to: (方法)
```Javascript
// 今日の日付を取得します
const today = new Date();

// 5日後の日付を計算します
const fiveDaysLater = new Date(today.getTime() + (5 * 24 * 60 * 60 * 1000));
console.log(fiveDaysLater.toString()); // 出力例: Mon Mar 07 2023 14:56:18 GMT+0900 (Japan Standard Time)

// 3週間前の日付を計算します
const threeWeeksEarlier = new Date(today.getTime() - (21 * 24 * 60 * 60 * 1000));
console.log(threeWeeksEarlier.toString()); // 出力例: Mon Feb 13 2023 14:56:18 GMT+0900 (Japan Standard Time)
```

## Deep Dive (深堀り)
JavaScriptの`Date`オブジェクトは、1970年1月1日00:00:00 UTCからのミリ秒数を基に日時を表します。過去や将来の日付を求めるには、現在のミリ秒数に変化させたい日数・時数・分数・秒数をミリ秒に換算して加算または減算します。

代替方法として`moment.js`のようなライブラリを使用することもできますが、簡単な計算であればネイティブの`Date`オブジェクトで充分です。しかし、タイムゾーンや夏時間のような複雑なケースを扱う場合、これらのライブラリが便利です。

また、`Date`オブジェクトでは月を0から数えることに注意が必要です（0 = 1月, 11 = 12月）。日付計算においてこの挙動を認識し、正しく処理することが肝心です。

## See Also (関連情報)
- JavaScript Date リファレンス: [MDN web docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- moment.jsライブラリ: [Moment.js](https://momentjs.com/)
- 日付と時刻を処理するためのluxonライブラリ: [Luxon](https://moment.github.io/luxon/#/)
- 日付の演算をバリデーションするためのDate-fnsライブラリ: [date-fns](https://date-fns.org/)
