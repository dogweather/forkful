---
title:                "Javascript: 日付を文字列に変換する"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することのメリットは、日付データを読みやすくし、特定の形式で表示することができることです。

## 使い方
日付を文字列に変換するには、 `toString()` メソッドを使用します。 `toString()` メソッドは、現在の時刻をデフォルトのロケールで文字列に変換します。例えば、次のコードブロックを参考にしてください。

```Javascript
let today = new Date(); // 現在の日付を取得
let stringDate = today.toString(); // 日付を文字列に変換
console.log(stringDate); // 結果: "Thu Oct 07 2021 20:10:32 GMT+0900 (Japan Standard Time)"
```

これだけで、日付が特定の形式で表示されます。さらに、`toLocaleDateString()` メソッドを使用することで、特定のロケールに応じた日付フォーマットに変換することもできます。例えば、次のコードブロックを参考にしてください。

```Javascript
let today = new Date(); // 現在の日付を取得
let stringDate = today.toLocaleDateString("ja-JP", { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }); // 日付を特定のロケールに応じたフォーマットに変換
console.log(stringDate); // 結果: "2021年10月7日" (日本語を使用する場合)
```

## その他の情報
日付を文字列に変換する際には、`toString()` メソッドと`toLocaleDateString()` メソッド以外にも、様々なメソッドやライブラリが存在します。例えば、Moment.jsやDate-fnsなどのライブラリを使用することで、より柔軟な日付フォーマットを実現することができます。

## おすすめのリンク
- [MDN - Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN - Date.prototype.toLocaleDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Moment.js公式サイト](https://momentjs.com/)
- [Date-fns公式サイト](https://date-fns.org/)