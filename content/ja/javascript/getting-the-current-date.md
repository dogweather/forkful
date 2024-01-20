---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:15:20.959683-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
JavaScriptで現在の日付を取得するのは基本です。アプリケーションに現実の時刻を反映させたり、ログを記録するためによく使われます。

## How to (やり方):
```Javascript
// 現在の日付と時刻を取得する
const now = new Date();

// 結果をコンソールに表示
console.log(now.toString()); // 例: Mon Mar 13 2023 17:45:00 GMT+0900 (Japan Standard Time)
```

## Deep Dive (詳細な解説):
JavaScriptで`Date`オブジェクトを作ると、その時点の日付と時刻が記録されます。`Date`は1970年代のUNIX時間が始まる以前からあります。代替方法として`Date.now()`, `performance.now()`やライブラリの使用がありますが、単純な使用では`new Date()`が最も一般的です。実装の詳細としては、ブラウザやサーバーのローカル時刻に依存することが挙げられます。

## See Also (関連リンク):
- MDN Web Docsの`Date`に関するドキュメント: [MDN Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- 日付操作ライブラリMoment.js: [Moment.js](https://momentjs.com/)
- 日付と時刻を操作するための新しいライブラリLuxon: [Luxon](https://moment.github.io/luxon/)
- 精度の高い時刻を測定する`performance.now()`: [MDN performance.now()](https://developer.mozilla.org/ja/docs/Web/API/Performance/now)