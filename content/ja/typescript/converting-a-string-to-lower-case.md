---
title:                "文字列を小文字に変換する"
html_title:           "TypeScript: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ文字列を小文字に変換するのか

文字列を小文字に変換することで、文字列の比較や検索が簡単になります。また、データの整形や表示においても、小文字に変換することで統一性が保たれ、見やすくなります。

## 方法

```TypeScript
const string = "Hello World";
const lowerCase = string.toLowerCase();
console.log(lowerCase);

// Output: "hello world"
```

TypeScriptでは、`toLowerCase()`メソッドを使用することで文字列を小文字に変換することができます。このメソッドはString型に組み込まれており、文字列を小文字に変換した新しい文字列を返します。変換前の文字列は変更されません。

## ディープダイブ

このメソッドはES6の標準であるため、TypeScriptでも使用することができます。また、英語以外の言語にも対応しています。さらに、`toUpperCase()`メソッドがあり、逆に大文字に変換することもできます。

## 関連記事

## リンク

- [String.prototype.toLowerCase() - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Stringのトード変換を行う toLowerCase() と replace() 関数 - EagletLab 技術ブログ](https://eagletmt.github.io/topic/2016/unicode-ie9.html)