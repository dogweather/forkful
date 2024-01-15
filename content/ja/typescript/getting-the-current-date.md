---
title:                "現在の日付を取得する"
html_title:           "TypeScript: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

はじめに

こんにちは、みなさん。今日はTypeScriptのプログラミングについてお話しします。特に、現在の日付を取得する方法について説明します。これは、新しいプロジェクトを始める際や、日付に関する機能を実装する際に非常に重要な知識です。それでは、さっそく始めましょう！

## Why

現在の日付を取得する理由は様々です。例えば、今日が何日か知りたい、特定の日付と比較してイベントをトリガーしたい、日付に関する処理を行いたいなどのケースがあります。また、日付を取得することで、アプリケーションやサービスの信頼性を高めることができます。

## How To

TypeScriptでは、現在の日付を取得するために様々な方法があります。まずは、Dateオブジェクトを使用する方法を紹介します。

```
TypeScript
const today = new Date(); // 現在の日付を取得
console.log(today); // 例: Wed Nov 04 2020 22:03:28 GMT+0900 (日本標準時)
```

Dateオブジェクトでは、`getFullYear()`、`getMonth()`、`getDate()`などのメソッドを使用することで、より詳細な日付情報を取得することができます。

```
TypeScript
const year = today.getFullYear(); // 現在の年を取得
const month = today.getMonth(); // 現在の月を取得 (0から11の数値で表される)
const date = today.getDate(); // 現在の日を取得
console.log(`${year}年${month + 1}月${date}日`); // 例: 2020年11月4日
```

また、moment.jsというライブラリを使用することで、より柔軟に日付を取得することができます。

```
TypeScript
import moment from 'moment'; // ライブラリをインポート

const today = moment(); // 現在の日付を取得
console.log(today); // 例: 2020-11-04T22:19:30+09:00

const year = moment().format('YYYY'); // 現在の年を取得
const month = moment().format('MM'); // 現在の月を取得
console.log(`${year}年${month}月`); // 例: 2020年11月
```

## Deep Dive

日付を取得するメソッドには、`Date.now()`や`moment().unix()`などもあります。これらはエポック秒と呼ばれる数値で返されます。これは、1970年1月1日午前0時からの経過秒数を表します。また、`new Date(timestamp)`や`moment.unix(timestamp)`といった形で、エポック秒を指定して特定の日付を作成することも可能です。

さらに、Dateオブジェクトやmomentオブジェクトでは、タイムゾーンを指定することで、ローカルタイムではなく他のタイムゾーンの日付を取得することもできます。これは、国際的なサービスを開発する際などに特に役立ちます。

## See Also

- [MDN | Date Object](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [TypeScript Handbook | Dates](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)

以上で、TypeScriptを使用して現在の日付を取得する方法