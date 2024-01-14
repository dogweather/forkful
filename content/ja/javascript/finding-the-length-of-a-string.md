---
title:    "Javascript: 文字列の長さを求める"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを見つけることに興味を持つ人のための記事です。

## 使い方

まず、文字列の変数を定義します。

```Javascript 
let str = "こんにちは、世界！"; 
```

次に、"length"メソッドを使用して文字列の長さを取得します。

```Javascript 
let length = str.length;
console.log(length); // 10
```

このように、文字列の長さを取得することができます。

## 詳細を深く掘り下げる

文字列の長さを取得する方法についてさらに詳しく説明します。JavaScriptでは、文字列は配列のように扱うことができます。つまり、文字列の各文字が配列の要素として扱われます。そのため、配列の長さを取得するために使われる"length"メソッドが文字列でも使うことができるのです。

また、この方法は単純な文字列だけではなく、変数や関数などの文字列も同様に長さを取得することができます。

## 参考リンク

[JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)  
[MDN Web Docs: String.prototype.length](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)  
[文字列の処理 (JavaScript)](https://www.javadrive.jp/javascript/string/)  

## 参考リンク