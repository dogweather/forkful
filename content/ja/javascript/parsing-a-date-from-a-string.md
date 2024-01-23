---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:36:57.389008-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
文字列から日付を解析することは、テキストを日付オブジェクトに変換するプロセスです。この技術は、ユーザー入力データの処理やAPIからの日付形式の統一によく使われます。

## How to (方法)
```javascript
// 日付文字列を解析して新しいDateオブジェクトを作成
const dateString = "2023-04-01T12:00:00.000Z";
const parsedDate = new Date(dateString);

console.log(parsedDate);
// 出力: Sat Apr 01 2023 13:00:00 GMT+0100 (Central European Summer Time)

// Dateオブジェクトを使って特定の情報を取得
console.log(parsedDate.getFullYear()); // 出力: 2023
console.log(parsedDate.getMonth());    // 出力: 3 (0 から始まる月)
console.log(parsedDate.getDate());     // 出力: 1
```

## Deep Dive (詳細な情報)
JavaScriptにおける日付の解析は、初期の段階から存在しています。`Date`オブジェクトはECMAScriptの初版から搭載されており、文字列から日付を生成するためによく使用されます。しかし、解析の結果はブラウザによって異なることがありますので、格納される日付の形式はISO 8601形式（例：`"2023-04-01T12:00:00.000Z"`）を使用するのが確実です。

代替手段として、ライブラリを使う方法（例：Moment.js、Date-fns、Day.jsなど）もあります。これらのライブラリは、日付の解析、検証、操作などを容易にし、ブラウザ間の一貫性を提供します。

更に`Date.parse()`メソッドもありますが、返されるのはミリ秒を表す数値であり、直接的な`Date`インスタンスを生成するわけではないので注意が必要です。国際化を考慮する場合、`Intl.DateTimeFormat`オブジェクトが便利です。

## See Also (関連情報)
- MDN Web Docs - Date: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Day.js: https://day.js.org/
