---
title:                "Javascript: 文字列を小文字に変換する"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

こんにちは、Javascriptプログラマーの皆さん！今日は、私たちがよく使用するJavaScriptの機能の1つである、文字列を小文字に変換する方法についてお話しします。

## なぜ文字列を小文字に変換するのか

文字列を小文字に変換することは、検索や比較を行う際に非常に便利です。例えば、ユーザーから入力された文字列の大文字や小文字の組み合わせが異なる場合でも、それらを一貫した形で処理することができます。また、大文字小文字を区別せずに文書を検索する場合にも役立ちます。

## 方法

文字列を小文字に変換するには、JavaScriptの組み込み関数`toLowerCase()`を使用します。この関数は、文字列の全ての文字を小文字に変換することができます。

```Javascript
let str = "Hello World!";
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr);

// 出力結果: hello world!
```

また、大文字や小文字の区別を無視して文字列を比較したい場合には、`toLowerCase()`の代わりに`localeCompare()`を使用する方法もあります。

```Javascript
let str1 = "apple";
let str2 = "APPLE";

if (str1.localeCompare(str2, undefined, {sensitivity: 'accent'}) === 0) {
  console.log("文字列は同じです");
} else {
  console.log("文字列は異なります");
}

// 出力結果: 文字列は同じです
```

## ディープダイブ

文字列を小文字に変換する際には、Unicode規格に基づいて変換が行われることに注意する必要があります。一見同じに見える文字でも、Unicodeの規則によっては異なる文字として扱われる場合があります。そのため、文字列を変換する際には、意図した変換が行われているかを常に確認するようにしましょう。

## 参考リンク

- [Javascriptの組み込み関数: `toLowerCase()`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicodeの規則について](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/Explorer_compatibility)
- [大文字小文字を無視して比較する方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)

## その他の記事

[JavaScriptの便利な機能について学ぶ](https://example.com/casual-javascript-programming-for-japanese-readers)