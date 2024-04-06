---
date: 2024-01-20 17:32:26.395264-07:00
description: "How to: (\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u306B\u3001JavaScript\u3068\
  TypeScript\u306F`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u3063\u3066\
  \u65E5\u4ED8\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u308C\u4EE5\u5916\u306B\u3082\u3001`moment.js`\u3084`date-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.688114-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u306B\u3001JavaScript\u3068TypeScript\u306F\
  `Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\
  \u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u4EE5\u5916\
  \u306B\u3082\u3001`moment.js`\u3084`date-fns`\u306A\u3069\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u304C\u3042\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u65E5\u4ED8\u51E6\
  \u7406\u3092\u7C21\u5358\u306B\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002TypeScript\u3067\u306F\u3001`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306E\
  \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u65E5\u4ED8\u3092\u52A0\u6E1B\
  \u7B97\u3057\u3001\u578B\u6CE8\u91C8\u306B\u3088\u308A\u3001\u30B3\u30FC\u30C9\u306E\
  \u610F\u56F3\u3092\u660E\u78BA\u306B\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (やり方)
```TypeScript
// 現在の日付を取得
const today: Date = new Date();

// 未来の日付を計算（3日後）
const threeDaysLater: Date = new Date(today);
threeDaysLater.setDate(today.getDate() + 3);
console.log(`3日後: ${threeDaysLater.toDateString()}`); // Sample Output: 3日後: Sun Apr 06 2023

// 過去の日付を計算（5日前）
const fiveDaysAgo: Date = new Date(today);
fiveDaysAgo.setDate(today.getDate() - 5);
console.log(`5日前: ${fiveDaysAgo.toDateString()}`); // Sample Output: 5日前: Tue Apr 01 2023
```

## Deep Dive (深掘り)
歴史的に、JavaScriptとTypeScriptは`Date`オブジェクトを使って日付を扱うことができます。これ以外にも、`moment.js`や`date-fns`などのライブラリがあり、より複雑な日付処理を簡単に行うことができます。TypeScriptでは、`Date`オブジェクトのメソッドを使用して日付を加減算し、型注釈により、コードの意図を明確にすることができます。

## See Also (関連情報)
- MDN Web Docs - Date: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date
- date-fns documentation: https://date-fns.org/docs/Getting-Started
- moment.js documentation: https://momentjs.com/docs/
