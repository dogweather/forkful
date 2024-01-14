---
title:                "Javascript: 今日の日付の取得"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする際に、現在の日付を取得することは非常に重要です。コードやアプリケーションを作成する際に、現在の日付を使用してデータを整理したり、イベントをスケジュールしたりする必要があるからです。

## 使い方

```Javascript
// 現在の日付を取得する方法
const currentDate = new Date();

// 日付のフォーマットを指定する方法
const options = { year: 'numeric', month: 'long', day: 'numeric' };
const formattedDate = currentDate.toLocaleDateString('ja-JP', options);

// 結果をコンソールに出力する
console.log(`今日の日付は ${formattedDate} です。`);
```

上記のコードで、現在の日付を取得する方法がわかります。JavaScriptのDateオブジェクトを使用することで、日付のフォーマットを自由に指定することもできます。`toLocaleDateString`メソッドは、指定したロケールで日付を文字列に変換するために使用されます。上記の例では、日本語のロケールとして`ja-JP`を使用しています。

## 深堀り

現在の日付は、コンピューター内でエポック時刻と呼ばれる数値として表されます。エポック時刻とは、1970年1月1日0時0分0秒（世界協定時UTC）からの経過秒数を表します。Dateオブジェクトの`getTime`メソッドを使用すると、エポック時刻を取得することができます。

また、`toLocaleDateString`メソッドの第二引数には、日付のフォーマットを指定するオブジェクトを渡すことができます。上記の例では、`year`、`month`、`day`のプロパティでそれぞれ年、月、日のフォーマットを指定しています。

## 参考リンク

- [Date - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date.prototype.toLocaleDateString() - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [エポック時刻 - Wikipedia](https://ja.wikipedia.org/wiki/%E3%82%A8%E3%83%9D%E3%83%83%E3%82%AF%E6%99%82%E5%88%BB)