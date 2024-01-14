---
title:    "Javascript: 日付を文字列に変換する"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
日付を文字列に変換する理由は何でしょうか？日付を文字列に変換することで、プログラマーは日付データをデータベースに保存したり、ユーザーに見やすい形式で表示したりすることができます。

## How To
日付を文字列に変換する方法について、具体的なコード例を交えながら説明します。
```Javascript
// 日付を取得
const date = new Date();

// フォーマットを指定して文字列に変換
const dateString = `${date.getFullYear()}年${date.getMonth()+1}月${date.getDate()}日`;

// 結果を出力
console.log(dateString);
// 2021年8月10日
```
上記のコードでは、DateオブジェクトからgetFullYear、getMonth、getDateというメソッドを使用して、年月日をそれぞれ取得し、文字列として結合しています。
また、このようにして得られる文字列はローカルのタイムゾーンに基づいています。もしUTCでの日付を表示したい場合は、getUTCFullYear、getUTCMonth、getUTCDateというメソッドを使用することができます。

## Deep Dive
日付を文字列に変換する際には、フォーマットを指定することが重要です。どのようなフォーマットを指定するかによって、表示される文字列が異なります。
例えば、前述のコードでは`getFullYear()`を使用していますが、これは4桁の年を表示します。もし2桁の年を表示したい場合は、`getYear()`を使用することができます。
その他にも、月や日の表示についても様々なオプションがあります。詳しい情報は[Dateオブジェクトのドキュメンテーション](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)を参照してください。

また、日付を文字列に変換する際には、タイムゾーンやロケールにも注意する必要があります。これらが異なる場合、日付の表示が異なる可能性があります。特に、国際的なアプリケーションを作成する場合は注意が必要です。

## See Also
- [Dateオブジェクトのドキュメンテーション](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [日付フォーマットの仕様についての記事](https://qiita.com/ykzn11/items/63eb6663a3f5a5720ed3)