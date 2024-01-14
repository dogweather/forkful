---
title:                "TypeScript: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由は、プログラミングで日付を扱う必要があるためです。日付を文字列に変換することで、文字列として日付を表示したり、他のデータと一緒に扱うことができます。

## 方法
日付を文字列に変換する方法はいくつかあります。例えば、DateオブジェクトのtoLocaleDateString()メソッドを使用することができます。以下のコード例をご覧ください。

```TypeScript
const date = new Date();
const dateString = date.toLocaleDateString();
console.log(dateString); // 出力： 2021/10/14
```

他にも、Moment.jsというライブラリを使用することもできます。以下のコード例をご覧ください。

```TypeScript
const date = new Date();
const dateString = moment(date).format("YYYY/MM/DD");
console.log(dateString); // 出力： 2021/10/14
```

## 深く掘り下げる
日付を文字列に変換する際、ユーザーの地域や言語に合わせてフォーマットする必要がある場合があります。例えば、日本語では「2021年10月14日」という表記が一般的ですが、英語では「October 14, 2021」という表記が一般的です。このような場合、toLocaleDateString()メソッドを使用すると、自動的に地域や言語に合わせたフォーマットを行ってくれます。

また、Moment.jsを使用する場合も、書式指定文字列によって日付を任意の形式に変換することができます。例えば、"ll"を指定すると、月の短縮形と日付を表すLLフォーマットで日付を表示することができます。

## See Also
- [MDN Web Docs - Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Documentation](https://momentjs.com/docs/)