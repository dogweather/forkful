---
title:    "Javascript: 現在の日付を取得する"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングで現在の日付を取得する理由はたくさんあります。例えば、ユーザーに投稿された内容を表示する際に、投稿日時を表示する必要があるかもしれません。または、予定や期限を管理するアプリケーションを作成する際に、現在の日付を取得してタスクを追加する機能を実装することができます。現在の日付を取得することで、様々な場面で便利に使うことができます。

## 方法

JavaScriptを使用して現在の日付を取得する方法を見ていきましょう。まずは```Date```オブジェクトを使って現在の日付を取得し、その後にフォーマットして表示する例を見てみましょう。

```javascript
const currentDate = new Date();
console.log(currentDate); // Output: 2021-01-01T00:00:00.000Z (例の日付と時間は実行時により異なる可能性があります。)
```

上記の例では、```Date```オブジェクトを使って現在の日付を取得しています。このオブジェクトには現在の日付だけでなく、日付を操作したり他の日付に変換する機能があります。

次に、フォーマットを変更して表示する例を見てみましょう。

```javascript
const currentDate = new Date();
const options = { year: "numeric", month: "long", day: "numeric" };
const formattedDate = currentDate.toLocaleDateString("ja-JP", options);
console.log(formattedDate); // Output: 2021年1月1日
```

上記の例では、```toLocaleDateString()```メソッドを使用してフォーマットを変更し、日本語で表示しています。このように、```Date```オブジェクトの様々な機能を駆使することで、さまざまな形式で現在の日付を表示することができます。

## ディープダイブ

```Date```オブジェクトには、その日付が何曜日かや、その年がうるう年かどうかなど、様々な情報を取得するメソッドがあります。また、時間や時差も取得することができます。詳細な情報は公式ドキュメントを参照してください。

## See Also

- [Dateオブジェクト - MDN 公式ドキュメント (日本語)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Dateオブジェクト - W3Schools (英語)](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [日付のフォーマット - JavaScript.info (英語)](https://javascript.info/date-formatting)