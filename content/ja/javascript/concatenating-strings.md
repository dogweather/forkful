---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、文字列を一つに結びつけるプログラミングの技術です。この技術は、異なる情報源から得られたデータを一つの完全なメッセージにまとめることを可能にします。

## どうやって：

以下に、JavaScriptで文字列を連結する基本的な方法を示します：

```Javascript
let str1 = "こんにちは、";
let str2 = "世界!";
let greeting = str1 + str2;
console.log(greeting);  //"こんにちは、世界!"
```

テンプレートリテラルを利用すると、さらに簡単に文字列を結合できます：

```Javascript
let planet = "世界";
let greeting = `こんにちは、${planet}!`;
console.log(greeting);  //"こんにちは、世界!"
```

## ディープダイブ：

文字列の連結は、プログラミングの歴史の初期段階から存在しています。JavaScriptでは、"+演算子"または "template literals"を使うことが一般的な方法です。

しかしながら、パフォーマンスを向上させるためのアルタナティブとして "StringBuilder" パターンの利用も考えられます。この実装は文字列の連結のコストを下げることが可能です。

```Javascript
let parts = ["こんにちは、", "世界!"];
let greeting = parts.join("");
console.log(greeting); //"こんにちは、世界!"
```

## 参考：

詳しく調べるための素晴らしいリソースをいくつか以下に紹介します：

1. [MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Operators/String_Operators): "+"演算子やテンプレートリテラルについて詳しく解説。
2. [JavaScript Info](https://javascript.info/string): JavaScriptの文字列関連の詳細な情報。