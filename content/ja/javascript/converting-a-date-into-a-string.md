---
title:    "Javascript: 日付を文字列に変換する"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
JavaScriptを使って日付を文字列に変換するのはなぜですか？

日付を文字列に変換することは、日付を見やすく表示したり、データベースに保存したり、あるいはその他の処理を行うために必要な場合があります。それでは、実際にどのように変換するのでしょうか？これから説明します。

## 方法
```Javascript
const today = new Date();
const stringDate = today.toDateString();
console.log(stringDate); // "Fri Apr 30 2021"
```

上記のように、Dateオブジェクトを作成し、その`toDateString()`メソッドを使うことで、日付を文字列に変換することができます。このように、JavaScriptではDateオブジェクトを使うことで、日付を文字列に変換することができます。

また、日付のフォーマットを変更したい場合は、`toLocaleDateString()`メソッドを使うことができます。例えば、日本語ロケールでのフォーマットは次のようになります。

```Javascript
const today = new Date();
const stringDate = today.toLocaleDateString('ja-JP', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
console.log(stringDate); // "金曜日, 2021年4月30日"
```

さらに、任意のフォーマットで日付を表示させたい場合は、ライブラリを使うこともできます。例えば、Moment.jsやdate-fnsなどがあります。

## ディープ・ダイブ
Dateオブジェクトのメソッドを使うことで、日付を文字列に簡単に変換することができますが、裏側ではどのように処理されているのでしょうか？

JavaScriptでは、Dateオブジェクトが内部的にはUnix時間（1970年1月1日からのミリ秒数）で日付を管理しています。そのため、Dateオブジェクトを使うことで、Unix時間を日付文字列に変換することができるのです。また、`toLocaleDateString()`メソッドを使うことで、ロケールに応じたフォーマットで日付を表示することができるのです。

## その他の参考リンク

[Dateオブジェクト - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
[Moment.js](https://momentjs.com/)
[date-fns](https://date-fns.org/)