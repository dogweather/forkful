---
title:                "Javascript: 文字列の大文字化"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# なぜ文字列を大文字にするのか

文字列の大文字化を行う理由はさまざまです。一般的な理由としては、入力された文字列をタイトルや見出しのように大文字で表示したい場合や、ユーザーの入力ミスを修正したい場合、あるいは入力された文字列をデータベースに保存する前に正規化したい場合などがあります。

## 方法

大文字化するには、標準のJavascript関数である`toUpperCase()`を使用します。これは、文字列のすべての文字を大文字に変換するというシンプルな機能を持っています。

```
Javascript
let str = "hello world";
console.log(str.toUpperCase()); // HELLO WORLD
```

また、正規表現を使用して、特定の文字列やパターンのみを大文字に変換する方法もあります。

```
Javascript
let str = "hello world";
console.log(str.replace(/world/gi, "WORLD")); // hello WORLD
```

このように、`toUpperCase()`を使用するか、正規表現を組み合わせて使用するかは、大文字化する文字列やパターンによって異なります。適切な方法を選択してください。

## ディープダイブ

`toUpperCase()`関数は、ES6以前のバージョンでも使用できるように定義されているプロトタイプメソッドです。しかし、内部でどのように動作しているかを知ることは重要です。

この関数は、ASCII文字列に対して正確に動作するように設計されていますが、Unicode文字列に対しては異なる結果をもたらす可能性があります。これは、Unicode文字列には複数のバージョンが存在するためで、例えばトルコ語では"i"が"İ"ではなく"i"に変換されます。

このようなケースでは、正規化やカスタム関数を使用し、より確実な結果を得ることができます。

## その他の参考記事

- [文字列の大文字化について学ぶ](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [文字列の大文字化と小文字化の違い](https://jisho.org/)
- [トルコ語での大文字化と小文字化の挙動](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase#workflow_of_toUpperCase)

### さらに見る

- [Unicodeについて学ぶ](https://unicode.org/)
- [正規化について学ぶ](https://www.regular-expressions.info/unicode.html)