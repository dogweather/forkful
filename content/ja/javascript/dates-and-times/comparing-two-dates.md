---
date: 2024-01-20 17:33:17.720846-07:00
description: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\uFF1F\u305D\u308C\
  \u306F2\u3064\u306E\u65E5\u4ED8\u304C\u3069\u3046\u95A2\u4FC2\u3057\u3066\u3044\u308B\
  \u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30A4\u30D9\u30F3\u30C8\u306E\u9806\u5E8F\u3092\u628A\u63E1\
  \u3057\u305F\u308A\u3001\u671F\u9650\u3092\u30C1\u30A7\u30C3\u30AF\u3057\u305F\u308A\
  \u3059\u308B\u305F\u3081\u306B\u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.802786
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\uFF1F\u305D\u308C\
  \u306F2\u3064\u306E\u65E5\u4ED8\u304C\u3069\u3046\u95A2\u4FC2\u3057\u3066\u3044\u308B\
  \u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30A4\u30D9\u30F3\u30C8\u306E\u9806\u5E8F\u3092\u628A\u63E1\
  \u3057\u305F\u308A\u3001\u671F\u9650\u3092\u30C1\u30A7\u30C3\u30AF\u3057\u305F\u308A\
  \u3059\u308B\u305F\u3081\u306B\u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を比較するって？それは2つの日付がどう関係しているかを確認することです。プログラマーはイベントの順序を把握したり、期限をチェックしたりするために日付を比較します。

## How to: (方法)
```javascript
// 日付オブジェクトを生成
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-10T00:00:00');

// 日付を比較
if(date1 < date2) {
  console.log('date1はdate2より前です。');
} else if(date1 > date2) {
  console.log('date1はdate2より後です。');
} else {
  console.log('date1とdate2は同じです。');
}

// 出力：date1はdate2より前です。
```

## Deep Dive (深掘り)
日付の比較は基本的に数値の比較です。JavaScriptでは内部的に日付はミリ秒で数えられているからです。この数値は1970年1月1日00:00:00 UTCからの経過ミリ秒数です。`getTime()` メソッドでこの値を取得できます。そうすればより明確な比較が可能です。また、ライブラリを使うともっと簡単です。例えばMoment.jsやDate-fnsは強力な日付処理機能を提供します。しかし、多くの場合、JavaScriptの標準機能で十分です。

## See Also (関連情報)
- MDN Web Docs: Dateオブジェクト - https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js公式サイト - https://momentjs.com/
- Date-fnsライブラリー - https://date-fns.org/
