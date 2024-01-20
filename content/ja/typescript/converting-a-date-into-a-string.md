---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 日付から文字列への変換 - TypeScriptプログラミング

## 何となぜ？
日付から文字列への変換とは、Date object型をString型に変換することです。なぜ必要かというと、日付データを人間が読む形式（例：'2022-03-01'）で表示したり、比較したりするためです。

## 方法
```TypeScript
const currentDate = new Date();
const dateString = currentDate.toISOString();
console.log(dateString);
```
これを実行すると、ISO 8601形式（例：'2022-03-01T13:00:00.000Z'）の文字列が表示されます。

## ディープダイブ
1. この方法はもともとJavaScriptにも存在し、2015年にECMAScript 6で正式に採用されました。TypeScriptはJavaScriptのスーパーセットなので、この機能が利用できます。
2. 代替手法として、日付の組み込みメソッド（getDate, getMonth, getYearなど）を用いて自分でフォーマットを作る方法があります。しかし、toISOStringメソッドは一般的な使用ケースで簡便性と読み易さを提供します。
3. 実装の詳細では、toISOStringメソッドはJavaScriptのDateプロトトタイプの一部です。このメソッドは現在の日時をUTC（協定世界時）に変換し、ISO 8601形式の文字列を返します。

## 参照情報
- ECMAScript 6に関する情報：https://learn.javascript.info/es6  
- Date toISOtringの仕様：https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString  
- Date objectの詳細な使い方：https://www.w3schools.com/js/js_date_methods.asp