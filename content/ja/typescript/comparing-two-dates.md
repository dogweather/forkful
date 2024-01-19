---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何？なぜ？

日付の比較とは、日付オブジェクト同士の大小を比べることです。これを行う理由は、イベントの期間を見積もったり、時間帯の違いを確認したり、即座に期限を設定するためです。

## 方法：

日付の比較は、Dateオブジェクトを利用することで簡単に行うことが出来ます：

```TypeScript
let date1 = new Date(2021, 5, 20);
let date2 = new Date(2021, 5, 25);

// 日付を比較
if(date1 > date2){
    console.log("date1 is later");
}
else if(date1 < date2){
    console.log("date2 is later");
}
else {
    console.log("Both dates are same");
}
```

このコードの結果、`"date2 is later"`が出力されます。

## 深掘り：

日付の比較を行う方法は、JavaScriptが登場して以来使われてきました。旧来の方法では、`getTime()`メソッドを使って日付を比較することが可能でした。しかしどちらの方法も同じ結果を出し、特別なケースに適用することはあまりありません。

```TypeScript
let date1 = new Date(2021, 5, 20);
let date2 = new Date(2021, 5, 25);

// getTimeを使って日期を比較
if(date1.getTime() > date2.getTime()){
    console.log("date1 is later");
}
else if(date1.getTime() < date2.getTime()){
    console.log("date2 is later");
}
else {
    console.log("Both dates are same");
}
```

このコードの結果も同様に、`"date2 is later"`が出力されます。

## 参照：

以下は日程の比較に関連するその他のリソースへのリンクです。

- [Mozilla Developer Network - JavaScript の Date オブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Stack Overflow - Date Comparison in TypeScript](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)