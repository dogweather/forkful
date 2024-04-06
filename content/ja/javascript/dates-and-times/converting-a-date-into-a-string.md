---
date: 2024-01-20 17:37:05.786878-07:00
description: "How to (\u65B9\u6CD5) \u521D\u671F\u306EJavaScript\u3067\u306F\u3001\
  \u65E5\u4ED8\u3092\u6271\u3046\u65B9\u6CD5\u304C\u975E\u5E38\u306B\u57FA\u672C\u7684\
  \u3067\u3057\u305F\u3002ECMAScript 5\u3067\u306F\u3001`Date.prototype.toISOString`\u306E\
  \u3088\u3046\u306A\u30E1\u30BD\u30C3\u30C9\u304C\u767B\u5834\u3057\u3001ISO\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.178855-06:00'
model: gpt-4-1106-preview
summary: "\u4EE3\u66FF\u65B9\u6CD5\u3068\u3057\u3066\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3082\u5B58\u5728\u3057\u307E\u3059\u3002Moment.js\u3001Date-fns\u3001Day.js\u306A\
  \u3069\u304C\u65E5\u4ED8\u306E\u51E6\u7406\u3084\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u306B\u7279\u5316\u3057\u305F\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\
  \u3053\u308C\u3089\u306F\u67D4\u8EDF\u6027\u304C\u9AD8\u304F\u3001\u5FA9\u96D1\u306A\
  \u65E5\u4ED8\u64CD\u4F5C\u304C\u5FC5\u8981\u306A\u5834\u5408\u306B\u6709\u7528\u3067\
  \u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to (方法)
```javascript
// 現在の日付を取得
const now = new Date();

// 標準のtoString()メソッドを使う
console.log(now.toString()); // "Wed Apr 05 2023 17:00:00 GMT+0900 (Japan Standard Time)"

// toISOString()メソッドで国際標準形式に
console.log(now.toISOString()); // "2023-04-05T08:00:00.000Z"

// toLocaleString()でローカライズされた日付
console.log(now.toLocaleString('ja-JP')); // "2023/4/5 17:00:00"
```

## Deep Dive (掘り下げ)
初期のJavaScriptでは、日付を扱う方法が非常に基本的でした。ECMAScript 5では、`Date.prototype.toISOString`のようなメソッドが登場し、ISO 8601形式で日付を文字列化する国際標準が提供されました。さらに、`Date.prototype.toLocaleString`はロケール情報に基づいた書式で表示でき、カスタムオプションも利用可能になりました。これらのメソッドは、ブラウザ間での一貫性とデータのグローバルな交換をサポートします。

代替方法としてのライブラリも存在します。Moment.js、Date-fns、Day.jsなどが日付の処理やフォーマットに特化した機能を提供します。これらは柔軟性が高く、復雑な日付操作が必要な場合に有用です。

実装の詳細では、`toString()`はブラウザやサーバーのタイムゾーン設定に依存しており、常に一貫した出力が得られるとは限りません。一方、`toISOString()`はUTC時間で一貫したISO形式を提供し、`toLocaleString()`はユーザーごとの表示設定の違いを吸収します。

## See Also (参照)
- [MDN Web Docs - Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Moment.js](https://momentjs.com/)
- [Date-fns](https://date-fns.org/)
- [Day.js](https://day.js.org/)
