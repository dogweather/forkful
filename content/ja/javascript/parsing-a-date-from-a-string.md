---
title:                "日期を文字列から解析する"
html_title:           "Javascript: 日期を文字列から解析する"
simple_title:         "日期を文字列から解析する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ?
日付を文字列から解析することとは、日付を文字列として表された形式から、プログラマーが理解しやすい形式に変換することです。この処理を行う理由は、プログラミングで日付を扱う必要があるためです。

## 方法:
```Javascript
const date = new Date("2021/07/16");
console.log(date);
// 出力: Fri Jul 16 2021 00:00:00 GMT+0900 (Japan Standard Time)
```

## 深く掘り下げる:
日付を文字列から解析することは、プログラマーにとって便利な手段です。これにより、データベースの日付や、APIから受け取った日付などを、プログラマーが必要とする形式に変換することができます。代替手段としては、外部ライブラリを使用する方法もあります。日付を文字列から解析する際の実装の詳細については、各プログラミング言語の公式ドキュメントを確認することができます。

## 関連リンク:
- [MDN Web Docs：Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [w3schools：JavaScript Date メソッド](https://www.w3schools.com/js/js_date_methods.asp)
- [JavaScript.info：Working with Dates](https://javascript.info/date)