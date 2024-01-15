---
title:                "現在の日付を取得する"
html_title:           "Javascript: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ日付を取得する必要があるのか

日付は、私たちの日常生活でとても重要なものです。仕事や予定を組む際には、常に日付を意識しています。特にプログラミングをする際は、日付を取得する必要があります。JavaScriptを使って現在の日付を取得する方法を紹介します。

## JavaScriptを使った日付の取得方法

まず、現在の日付を取得するには、Dateオブジェクトを使用します。以下のコードを参考にしてください。

```javascript
let currentDate = new Date(); // 現在の日付を取得
console.log(currentDate); // 例: Tue Sep 21 2021 12:33:26 GMT-0400 (Eastern Daylight Time)
```

このように、Dateオブジェクトを作成すると、現在の日付が取得できます。ただし、使用するコンピューターのタイムゾーンを考慮する必要があることに注意してください。

さらに、日付を指定した書式で取得することもできます。以下のコードを参考にしてください。

```javascript
let currentDate = new Date();
let year = currentDate.getFullYear(); // 現在の年を取得
let month = currentDate.getMonth() + 1; // 月は0から始まるので、+1する
let day = currentDate.getDate(); // 日を取得

console.log(`${year}年${month}月${day}日`); // 例: 2021年9月21日
```

Dateオブジェクトには、年・月・日の他にも時間や曜日など、さまざまな情報が含まれています。必要に応じて、取得するデータを調整してください。

## 日付の取得についての詳細

Dateオブジェクトの詳細については、公式ドキュメントを参照することで深く理解することができます。また、JavaScript以外の言語でも、似たような方法で日付を取得することができるので、興味のある方はぜひ調べてみてください。

## See Also

- [Dateオブジェクトについて (MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScriptで日付を操作する方法 (Qiita)](https://qiita.com/koseig/items/77c525ba15f397a7021c)