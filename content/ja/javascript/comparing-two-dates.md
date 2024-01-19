---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付の比較とは、二つの日付がどちらが先、または遅いかを判断することです。これはプログラマが時間経過に基づくイベントを制御するために必要です。

## どうやって:

JavaScriptで二つの日付を比較する一番簡単な方法を見てみましょう。

``` Javascript
let date1 = new Date("2021-12-05");
let date2 = new Date("2022-01-01");

if(date1 > date2) {
  console.log("date1 is later than date2");
} else if(date1 < date2) {
  console.log("date1 is earlier than date2");
} else {
  console.log("Both dates are equal");
}
```
このコードを実行すると、"date1 is earlier than date2"が出力されます。

## ディープダイブ:

1. 歴史:

JavaScriptの初期バージョンでは、日付の比較は直感的ではありませんでした。しかし、ES5で導入されたDateオブジェクトはこの問題を解決しました。

2. 代替手段:

ライブラリを使用することも選択肢の一つです。例えばMoment.jsは日付管理をより便利にします。

3. 実装詳細:

上記のコードでは、日付は内部的にミリ秒として管理されており、数値として比較されます。これが直接日付の比較を可能にします。

## 参考情報:

- [MDN Web Docs - Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [デイト（日付）オブジェクトの詳細](https://javascript.info/date)