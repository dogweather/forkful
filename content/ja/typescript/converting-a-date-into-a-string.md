---
title:                "日付を文字列に変換する"
html_title:           "TypeScript: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何が望ましいか？

日付を文字列に変換することは、プログラマーが日付を表す値をテキストとして扱いやすくするために行われる作業です。例えば、データベースに保存されている日付を画面に表示する場合や、ユーザーが入力した日付を処理する場合などに、日付を文字列に変換する必要があります。

## 方法：

日付を文字列に変換するには、TypeScriptでデフォルトで提供されるDateオブジェクトのメソッドを使用します。例えば、次のコードでは、現在の日付を "yyyy-mm-dd" の形式で文字列に変換しています。

```TypeScript
let date = new Date();
let dateString = date.toISOString().split('T')[0]; // 例： "2020-09-15"
console.log(dateString); // 出力："2020-09-15"
```

もし、月の名前を表すテキストを使いたい場合は、別のメソッドである ```toLocaleDateString()``` を使用します。

```TypeScript
let month = date.toLocaleDateString('ja-JP', { month: 'long' }); // 例： "9"
```

このように、必要に応じて、日付を細かく制御することができます。

## 深く掘り下げる：

日付を文字列に変換する方法は、プログラミング言語や環境によって異なりますが、基本的な考え方や手法は同じです。最も古い時代から、日付を文字列に変換する方法は、タイムスタンプとして知られる数値を使うことでした。しかし、現代のプログラミング言語では、Dateオブジェクトが提供する便利なメソッドを使うことができます。また、必要に応じて特定のタイムゾーンやロケールを指定することもできます。

このように、日付を文字列に変換する方法は、プログラマーにとって重要な機能の一つであり、正しく理解することが大切です。

## 関連リンク：

- [TypeScript Dateオブジェクトドキュメント] (https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [MDN Dateオブジェクトドキュメント] (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)