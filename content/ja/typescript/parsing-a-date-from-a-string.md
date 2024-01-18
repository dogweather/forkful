---
title:                "文字列から日付を解析する"
html_title:           "TypeScript: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何？なぜ？
日付を文字列から解析することとは、日付を別の形式に変換することです。プログラマーがこのようなことを行う理由は、データを整形したり、データベースやネットワークに送信するために必要だからです。

## 方法：
TypeScriptを使用して、文字列から日付を解析する方法は2種類あります。

```TypeScript
// 方法１：Dateオブジェクトを使用する方法
let dateString = "2020/10/01";
let date = new Date(dateString);
console.log(date); // 結果：Thu Oct 01 2020 00:00:00 GMT+0900 (JST)

// 方法２：DatePipeを使用する方法
import { DatePipe } from '@angular/common';
let dateString = "2020-10-01";
let date = new Date(dateString);
let formattedDate = new DatePipe('en-US').transform(date, 'yyyy/MM/dd');
console.log(formattedDate); // 結果：2020/10/01
```

## 詳細：
日付を文字列から解析する必要が出てきた背景として、インターネットの発達による国際的なやりとりの増加が挙げられます。さまざまな国や地域で異なる日付のフォーマットがあり、それらを統一するために日付を解析する必要が生じました。また、日付をデータベースやネットワークで受け渡す際には、文字列から解析することが必要です。

日付を解析する他の方法としては、正規表現を使用する方法やサードパーティ製のライブラリを使用する方法があります。しかし、TypeScriptを使用すると、標準のDateオブジェクトやDatePipeを使用することで、簡単に日付を解析することができます。

## 関連情報：
- [TypeScript公式ドキュメント - Dateオブジェクト](https://www.typescriptlang.org/docs/handbook/standard-built-in-types.html#date)
- [Angular公式ドキュメント - DatePipe](https://angular.io/api/common/DatePipe)