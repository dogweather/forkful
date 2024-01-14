---
title:    "Javascript: 未来や過去の日付を計算する"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

日付を計算するのにどのようなメリットがありますか？
これは、将来や過去の特定の日付を計算する必要がある場合に便利です。例えば、何日後の誕生日を知りたい場合や、日付間の期間を計算したい場合などに役立ちます。

## 使い方

日付を計算するには、JavascriptのネイティブなDateオブジェクトを使用します。このオブジェクトには、指定した日付や時間を表すプロパティやメソッドが用意されています。

例えば、今日から1ヶ月後の日付を計算する場合、以下のようにコードを書くことができます。

```Javascript
let today = new Date(); // 現在の日付を取得
let future = new Date(today.getFullYear(), today.getMonth() + 1, today.getDate()); // 今日の日付から1ヶ月後の日付を計算
console.log(future); // 結果： "2020-04-11T00:00:00.000Z"
```

このように、Dateオブジェクトを使用することで簡単に日付の計算ができます。他にも、さまざまな式を組み合わせることで、複雑な日付の計算も可能です。

## 詳細について

Dateオブジェクトには、日付や時間を操作するためのメソッドが用意されています。例えば、`getDate()`メソッドを使用すると、現在の日付から何日後の日付を取得することができます。

また、`setDate()`メソッドを使うと、特定の日付や時間を設定することもできます。これらのメソッドを組み合わせることで、さまざまな日付の計算や操作が可能になります。

さらに、Moment.jsなどの外部のライブラリを使用することで、より高度な日付の操作が可能になります。これらのライブラリには、異なるタイムゾーンや多言語に対応する機能も備わっています。

## 参考

[Dateオブジェクト | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)

[Moment.js](https://momentjs.com/)

[日付計算を行う | Qiita](https://qiita.com/hmsk/items/fc6cf4976e0beae8f328)