---
title:                "2つの日付の比較"
html_title:           "TypeScript: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

##
何となく理解できることを言いますと、2つの日付を比較することは、その日付が同じかどうか、どちらが前の日付か、どちらが後の日付かを確認することです。プログラマーがこれを行う理由は、例えばゲームの進行やファイルの更新日時を管理するために必要だからです。

## 
TypeScriptを使って2つの日付を比較する方法はいくつかあります。例えば、```date1 > date2```というように直接比較する方法や、```if (date1.getTime() > date2.getTime())```というようにそれぞれの日付のミリ秒表現を比較する方法があります。下記のコードブロックを参考にしてください。

```
const date1 = new Date("2021/01/01");
const date2 = new Date("2021/01/02");

console.log(date1 > date2); // Output: false
console.log(date1.getTime() > date2.getTime()); // Output: false
```

## 
日付の比較は、昔からプログラミングでよく使われてきました。現在では、日付の比較のためのライブラリやAPIも多くあります。例えば、Moment.jsやJavaScriptのDateオブジェクトなどがあります。プログラマーは自分の使いやすい方法で日付の比較を行うことができます。

## 
関連情報については下記リンクを参考にしてください。
- [Moment.js](https://momentjs.com/)
- [JavaScript Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)