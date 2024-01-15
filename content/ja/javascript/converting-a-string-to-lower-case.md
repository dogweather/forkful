---
title:                "「文字列を小文字に変換する」"
html_title:           "Javascript: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換するのに従事する理由は、ユーザーの入力やデータベースからの情報を処理する際に必要になるからです。例えば、ユーザーが大文字で入力した場合でも、正しい結果を返すようにするためです。

## 方法

文字列を小文字に変換する方法は簡単です。まず、小文字にしたい文字列を ```toLowerCase()``` メソッドで囲みます。例えば:

```Javascript
let str = "Hello World";
let lowerStr = str.toLowerCase();

console.log(lowerStr); // 出力: "hello world"
```

## さらに深く

文字列を小文字に変換する際、JavaScriptはUnicodeマッピングを使用します。これは、各文字に対応する小文字形式を指定するものです。また、```toLowerCase()``` メソッドは、英語以外の言語の文字も適切に変換することができます。

## 関連記事

- [JavaScriptのStringメソッドについて](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- [JavaScriptで文字列を操作する方法](https://qiita.com/warapuri1203/items/f32b825e8432667a0bdb)