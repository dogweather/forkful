---
title:                "日付を文字列に変換する"
html_title:           "Javascript: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 日付を文字列に変換するとは？

日付を文字列に変換するとは、プログラマーが日付を表示するために使うテクニックです。コンピューターやプログラミング言語では、日付は数字で表されていますが、人間にとって読みやすい形式に変換する必要があります。そこで、日付を文字列に変換するプログラムを作成します。

プログラマーが日付を文字列に変換する理由は、日付を人間が読みやすい形式で表示するためです。例えば、データベースに保存されている日付をユーザーが理解しやすい形式で表示する場合などに使われます。また、日付を文字列に変換することで、特定のフォーマットに準拠するようにすることができます。

## 方法：

```Javascript
// 日付オブジェクトを作成
let date = new Date();

// 日付を文字列に変換
let dateString = date.toString();
console.log(dateString); //出力："Fri Apr 02 2021 15:30:00 GMT+0900 (日本標準時)"

// 特定のフォーマットで日付を文字列に変換
let formattedDate = date.toLocaleDateString('ja-JP', {year: 'numeric', month: 'short', day: 'numeric'});
console.log(formattedDate); //出力："2021年4月2日"
```

## 深堀り：

日付を文字列に変換する方法は、プログラミング言語や環境によって異なります。歴史的には、プログラマーは日付を手動でフォーマットしていましたが、現在では日付オブジェクトに搭載された便利なメソッドを使うことで簡単に日付を文字列に変換することができます。

代替手段として、簡易的なライブラリや外部のAPIを使って日付を文字列に変換する方法もあります。また、日付を文字列に変換する際には、タイムゾーンやロケールを考慮することが重要です。

## 関連リンク：

- [MDN Web Docs - Date.prototype.toLocaleDateString()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [JavaScript Info - 日期と時間](https://ja.javascript.info/date)
- [日付と時刻を操る Date オブジェクト（JavaScript）](https://dev.classmethod.jp/articles/date-object-in-javascript/)