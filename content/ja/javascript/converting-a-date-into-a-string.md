---
title:                "Javascript: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することに関する記事を書くことの理由は、日付をより理解し、JavaScriptプログラムでより有効に使用するためです。ここでは、日付データ型を文字列に変換する方法を紹介します。

## 方法

日付を文字列に変換する方法には、さまざまなアプローチがあります。最も一般的な方法は、Dateオブジェクトの`toString()`メソッドを使用することです。このメソッドは、日付を文字列に変換して返します。

```JavaScript
const date = new Date();
const dateString = date.toString();
console.log(dateString); // "Tue Apr 13 2021 12:00:00 GMT+0900 (GMT+09:00)"
```

これにより、可読性の高い形式で日付が出力されます。しかし、必要に応じてよりカスタマイズした形式の文字列を取得したい場合は、さらに多くのオプションがあります。例えば、`getDate()`、`getMonth()`、`getFullYear()`などのメソッドを使用することで、日付の特定の部分だけを取得することができます。

```JavaScript
const date = new Date();
const dateText = `${date.getMonth() + 1}/${date.getDate()}/${date.getFullYear()}`;
console.log(dateText); // "4/13/2021"
```

また、外部ライブラリであるMoment.jsを使用することで、より簡単に日付をフォーマットしたり、ロケールに合わせた表示を行ったりすることができます。

## 深堀り

日付を文字列に変換する際には、主に2つのポイントに留意する必要があります。1つは、日付をどのようなフォーマットで表示するかということです。上で紹介したように、仕様に応じて日付オブジェクトのメソッドを使用するか、外部ライブラリを使うかを考える必要があります。

そしてもう1つは、タイムゾーンの扱いです。`toString()`メソッドはデフォルトで世界協定時刻（UTC）を基準として日付を表示します。そのため、必要に応じてタイムゾーンを設定する必要があります。Moment.jsでは、タイムゾーンを指定するための便利なメソッドが用意されています。

## See Also

- [JavaScriptのDateオブジェクトのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.jsの公式ドキュメント](https://momentjs.com/)