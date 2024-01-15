---
title:                "「日付を文字列に変換する」"
html_title:           "Javascript: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの最も一般的な理由は、ユーザーにとって読みやすい形式で日付を表示することです。

## 作り方
日付を文字列に変換するためには、Dateオブジェクトのメソッドである`toLocaleDateString()`を使用します。以下に例を示します。

```Javascript
let currentDate = new Date();
let dateString = currentDate.toLocaleDateString('ja-JP');
console.log(dateString); // 出力結果：'2021/1/1'
```

このように、`toLocaleDateString()`メソッドに言語の指定をすることで、指定された言語に応じた日付の文字列が返されます。

## 深堀り
日付を文字列に変換する際、`toLocaleDateString()`メソッドには3つの引数を設定することができます。引数の1つ目は言語の指定、2つ目は地域の指定、3つ目はオプションの設定です。引数を指定しない場合、デフォルトの言語と地域が使用されます。

また、日付を表す文字列には多様な形式があります。例えば、"2021年1月1日"や"1/1/2021"などがあります。このため、`toLocaleDateString()`メソッドは引数の設定によって日付の表記を変更することができます。

## 詳しくは
Mozilla Developer Networkの[Date.prototype.toLocaleDateString()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)を参考にしてください。

## 参考
- [Date.prototype.toLocaleDateString() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [日付を文字列に変換する - Qiita](https://qiita.com/nikuni_takasi/items/fe84d260e7545014c4ab)