---
title:                "TypeScript: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングにおいて現在の日付を取得する必要がある理由を簡単に説明します。

人々はしばしば、日付を記録したり、期限を設定したり、特定の日付と比較したりする必要があります。それゆえ、現在の日付を取得することは、より効率的にタスクを実行する上で重要な役割を果たします。

## 方法
TypeScriptを使用して、現在の日付を取得する方法を以下のコーディング例と共に紹介します。

```TypeScript
// 現在の日付を新しいDateオブジェクトとして取得する
let currentDate = new Date();

// 年、月、日を取得する
let year = currentDate.getFullYear();
let month = currentDate.getMonth() + 1; // 0から始まるため、+1する必要がある
let day = currentDate.getDate();

// 現在の日付を指定したフォーマットで表示する
let formattedDate = `${month}/${day}/${year}`;
console.log(formattedDate); // 10/1/2021と表示される
```

このように、Dateオブジェクトを使用することで簡単に現在の日付を取得し、必要に応じてフォーマットすることができます。

## ディープダイブ
DateオブジェクトはJavaScriptの組み込みオブジェクトの一つであり、日付や時刻を扱うために便利なメソッドを多数持っています。

例えば、現在の時刻を取得するには`getHours()`や`getMinutes()`といったメソッドが使用できます。また、他の日付と比較する際には`getTime()`を使用することで、日付を数値として取得することができます。

さらに、Dateオブジェクトを使用すると、タイムゾーンの設定や夏時間の考慮も自動的に行うことができます。

## 併せて読みたい
- [TypeScript公式ドキュメント：Dateオブジェクト](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#date-and-time-types)
- [MDN：Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScriptで現在の日時を扱う方法](https://qiita.com/teradonburi/items/e87b71e0803072b53c17)（日本語）