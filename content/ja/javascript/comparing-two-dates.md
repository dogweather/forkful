---
title:                "Javascript: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することの意義は、日常生活における重要な役割を果たしています。例えば、プログラミングをする際に、特定の日付の前後関係を知ることが必要になることがあります。また、データベースやスプレッドシートなどの操作をする際にも、日付を比較することがよくある作業です。

## 使い方

日付を比較する方法は複数ありますが、ここでは`Date()`オブジェクトを用いた方法を紹介します。まずは、比較したい日付をそれぞれの変数に保存します。

```Javascript
var date1 = new Date('2020/01/01');
var date2 = new Date('2020/05/05');
```

次に、`getTime()`メソッドを用いて、日付をミリ秒数に変換します。これにより、日付を数値として比較することができます。

```Javascript
var time1 = date1.getTime();
var time2 = date2.getTime();
```

最後に、比較するためのif文を使用し、日付の前後関係を調べることができます。

```Javascript
if (time1 > time2) {
  console.log('date1 is later than date2');
} else if (time1 < time2) {
  console.log('date1 is earlier than date2');
} else {
  console.log('both dates are the same');
}
```

上記の場合、`date1`は`date2`よりも後の日付なので、「date1がdate2よりも後の日付である」というメッセージが出力されます。

## 深層

日付を比較する際には、特定の精度によって正確な結果が得られることが重要です。例えば、`getTime()`メソッドはミリ秒単位までしか比較ができないため、秒以下の精度まで比較したい場合は、別の方法を用いる必要があります。

また、`Date()`オブジェクトの入力形式にも注意が必要です。例えば、月と日の順番が異なるフォーマットなどがあり、正しく比較するためには入力形式を厳密に合わせる必要があります。

さらに、時差やサマータイムなどの影響も考慮しなければなりません。より正確な結果を得るためには、そのような要素も考慮することが重要です。

## 参考リンク

- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Qiita: JavaScriptで日付を比較する方法](https://qiita.com/ut0n/items/be4c1136f7a7abf36f6a) 

## 関連ページ