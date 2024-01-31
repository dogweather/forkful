---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:32:26.395264-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

category:             "TypeScript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (なにを？どうして？)
日付の計算は、過去または未来の特定の日付を求めることです。プログラマは、予定の管理、期限の追跡、経過時間の計算などに日付計算を利用します。

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
