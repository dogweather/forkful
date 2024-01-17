---
title:                "日付の比較方法"
html_title:           "Javascript: 日付の比較方法"
simple_title:         "日付の比較方法"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となんで？

日付を比較するとは何か？プログラマーがそれをする理由は何か？

日付を比較するとは、二つの日付の間の関係を判断することです。プログラマーは、日付を比較することで、特定の日付が他の日付よりも前後するのかを知ることができます。

## 方法：

```Javascript
// 二つの日付を定義する
let date1 = new Date("2020-01-01");
let date2 = new Date("2019-12-31");

// 日付を比較し、結果を出力する
if (date1 > date2) {
  console.log("date1はdate2よりも後の日付です");
} else if (date1 < date2) {
  console.log("date1はdate2よりも前の日付です");
} else {
  console.log("date1とdate2は同じ日付です");
}
```

出力：
```Javascript
date1はdate2よりも後の日付です
```

## 深堀り：

歴史的文脈、代替手段、比較する日付の実装の詳細などを含む情報。

日付を比較することは、古いバージョンのJavascriptでは難しい作業でしたが、ECMAScript 5からは```Date```オブジェクトのメソッドを使用して簡単に比較することができるようになりました。代替手段として、日付を整数に変換してから比較する方法もあります。また、日付のタイムスタンプを比較することもできます。

## 関連情報：

- [MDNの「Date」オブジェクトのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [日付比較を行う際のベストプラクティス](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)