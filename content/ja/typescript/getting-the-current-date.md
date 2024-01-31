---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:17:09.244852-07:00
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
現在の日付を取得するって、文字通りの意味だ。何日今日か知るため、ログをつけたり、ユーザーに日付関連の情報を提供するためにプログラマはこれを行う。

## How to: (方法)
```TypeScript
// 現在の日付と時刻を取得
const now = new Date();
console.log(now);

// 出力例 (実際の出力は実行時の日付と時刻による)
// 2023-04-15T16:20:30.157Z
```

## Deep Dive (深掘り)
JavaScript の `Date` オブジェクトは1995年の言語誕生時からある。`new Date()` の単純な構文は、ブラウザやNode.js環境でも同じように動作する。代替として `Date.now()` で現在のミリ秒単位のタイムスタンプを取得できるが、 `new Date()` は日付と時刻の完全な表現を得るには適している。タイムゾーンの取り扱いや細かいフォーマット変更が必要な場合、 `Intl` オブジェクトや外部ライブラリ（例：Moment.js、Day.js）を使うこともある。

## See Also (関連情報)
- MDN Web Docs on Date: [Date - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- TypeScript Handbook: [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- Date-fns Library: [Date-fns - modern JavaScript date utility library](https://date-fns.org/)
- Day.js Library: [Day.js - 2KB Immutable date library alternative to Moment.js](https://day.js.org/)
