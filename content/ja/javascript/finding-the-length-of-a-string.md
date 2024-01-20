---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを見つけるとは、文字列の中に含まれる文字の数を確認することをいう。プログラマーは、文字列を扱いやすく、エラーを減らすためにこれを行います。

## 方法:

Javascriptで文字列の長さを見つける基本的な方法は、`.length` プロパティを使うことです。

```Javascript

let str = "こんにちは、ワールド！";
console.log(str.length);  //出力：14

```

このコードの結果は `14` で、これは "こんにちは、ワールド！" の文字数を表しています。

## ディープダイブ：

- ### 歴史的な文脈

Javascriptの `.length` プロパティは、ECMAScriptスペックの最初のバージョンから存在しています。これは文字列操作の基本であり、多くの他の言語にも見られます。

- ### 代替手段

場合によっては、特定の文字やパターンの数をカウントすることもあります。その場合、 `.match()` または `.split()` メソッドと正規表現を組み合わせて使用することができます。

```Javascript
let str = "こんにちは、ワールド！こんにちは、ワールド！";
let count = (str.match(/こんにちは/g) || []).length;
console.log(count); //出力：2
```

- ### 実装の詳細

 `.length` プロパティは、文字列が持つ `16ビット符号なし整数の配列` としての文字の数を返します。Javascriptでは、これは1から`2^53 - 1`までの値を表現することができます。

## 参考文献:

- [MDN 文字列の長さ](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [ECMAScript specification](https://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.1)