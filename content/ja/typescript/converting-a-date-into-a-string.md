---
title:                "「日付を文字列に変換する」"
html_title:           "TypeScript: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ日付を文字列に変換するのか

日付を文字列に変換することは、よくあるプログラミングタスクの一つです。例えば、データベースから取得した日付をユーザーが理解しやすい形式で表示したい場合や、フォームに入力された日付をバリデーションする際に使用します。日付を文字列に変換することで、より扱いやすい形式にデータを変換することができます。

## 方法

以下のコードブロックには、TypeScriptで日付を文字列に変換する方法の例が記載されています。

```TypeScript
const today = new Date(); // 現在の日付を取得
const dateString = today.toDateString(); // toDateString()メソッドを使用して日付を文字列に変換
console.log(dateString); // 結果: "Mon Jul 19 2021"
```

また、以下のようにフォーマット指定できる`toLocaleDateString()`メソッドもあります。

```TypeScript
const date = new Date(2021, 6, 19); // デフォルトの日付を設定
const options = { year: 'numeric', month: 'long', day: 'numeric' }; // フォーマット指定オプションを設定
const dateString = date.toLocaleDateString('en-US', options); // toLocaleDateString()メソッドを使用して日付をフォーマット
console.log(dateString); // 結果: "July 19, 2021"
```

## ディープダイブ

日付を文字列に変換する際に使用する主なメソッドは、`toDateString()`と`toLocaleDateString()`です。これらのメソッドはDateオブジェクトのプロトタイプに定義されており、日付を表示する際に使用するロケールやフォーマットを指定することができます。また、`toString()`メソッドも日付を文字列に変換することができますが、フォーマットが一定ではないため、変換後の文字列を信頼することはできません。

## 関連リンクを参照

[Date - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)