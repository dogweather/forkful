---
date: 2024-01-20 17:33:17.720846-07:00
description: "How to: (\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6BD4\u8F03\u306F\u57FA\u672C\
  \u7684\u306B\u6570\u5024\u306E\u6BD4\u8F03\u3067\u3059\u3002JavaScript\u3067\u306F\
  \u5185\u90E8\u7684\u306B\u65E5\u4ED8\u306F\u30DF\u30EA\u79D2\u3067\u6570\u3048\u3089\
  \u308C\u3066\u3044\u308B\u304B\u3089\u3067\u3059\u3002\u3053\u306E\u6570\u5024\u306F\
  1970\u5E741\u67081\u65E500:00:00 UTC\u304B\u3089\u306E\u7D4C\u904E\u30DF\u30EA\u79D2\
  \u6570\u3067\u3059\u3002`getTime()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.479769-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6BD4\u8F03\u306F\u57FA\u672C\u7684\u306B\
  \u6570\u5024\u306E\u6BD4\u8F03\u3067\u3059\u3002JavaScript\u3067\u306F\u5185\u90E8\
  \u7684\u306B\u65E5\u4ED8\u306F\u30DF\u30EA\u79D2\u3067\u6570\u3048\u3089\u308C\u3066\
  \u3044\u308B\u304B\u3089\u3067\u3059\u3002\u3053\u306E\u6570\u5024\u306F1970\u5E74\
  1\u67081\u65E500:00:00 UTC\u304B\u3089\u306E\u7D4C\u904E\u30DF\u30EA\u79D2\u6570\
  \u3067\u3059\u3002`getTime()` \u30E1\u30BD\u30C3\u30C9\u3067\u3053\u306E\u5024\u3092\
  \u53D6\u5F97\u3067\u304D\u307E\u3059\u3002\u305D\u3046\u3059\u308C\u3070\u3088\u308A\
  \u660E\u78BA\u306A\u6BD4\u8F03\u304C\u53EF\u80FD\u3067\u3059\u3002\u307E\u305F\u3001\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\u3068\u3082\u3063\u3068\u7C21\u5358\
  \u3067\u3059\u3002\u4F8B\u3048\u3070Moment.js\u3084Date-fns\u306F\u5F37\u529B\u306A\
  \u65E5\u4ED8\u51E6\u7406\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3057\
  \u304B\u3057\u3001\u591A\u304F\u306E\u5834\u5408\u3001JavaScript\u306E\u6A19\u6E96\
  \u6A5F\u80FD\u3067\u5341\u5206\u3067\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

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
