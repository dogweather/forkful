---
date: 2024-01-20 17:31:33.014965-07:00
description: "How to: (\u65B9\u6CD5) JavaScript\u306E`Date`\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u306F\u30011970\u5E741\u67081\u65E500:00:00 UTC\u304B\u3089\u306E\u30DF\
  \u30EA\u79D2\u6570\u3092\u57FA\u306B\u65E5\u6642\u3092\u8868\u3057\u307E\u3059\u3002\
  \u904E\u53BB\u3084\u5C06\u6765\u306E\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u306B\u306F\
  \u3001\u73FE\u5728\u306E\u30DF\u30EA\u79D2\u6570\u306B\u5909\u5316\u3055\u305B\u305F\
  \u3044\u65E5\u6570\u30FB\u6642\u6570\u30FB\u5206\u6570\u30FB\u79D2\u6570\u3092\u30DF\
  \u30EA\u79D2\u306B\u63DB\u7B97\u3057\u3066\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\
  \u3057\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.182321-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) JavaScript\u306E`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306F\u30011970\u5E741\u67081\u65E500:00:00 UTC\u304B\u3089\u306E\u30DF\u30EA\u79D2\
  \u6570\u3092\u57FA\u306B\u65E5\u6642\u3092\u8868\u3057\u307E\u3059\u3002\u904E\u53BB\
  \u3084\u5C06\u6765\u306E\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u306B\u306F\u3001\u73FE\
  \u5728\u306E\u30DF\u30EA\u79D2\u6570\u306B\u5909\u5316\u3055\u305B\u305F\u3044\u65E5\
  \u6570\u30FB\u6642\u6570\u30FB\u5206\u6570\u30FB\u79D2\u6570\u3092\u30DF\u30EA\u79D2\
  \u306B\u63DB\u7B97\u3057\u3066\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\u3057\u307E\
  \u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
