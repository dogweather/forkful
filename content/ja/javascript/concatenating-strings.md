---
title:                "文字列の連結"
date:                  2024-01-20T17:35:07.788160-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、二つ以上の文字列をつなげて一つにすることです。プログラマーはデータを組み合わせたり、ユーザーに表示したりするためにこれを行います。

## How to: (方法)
JavaScriptで文字列を連結する基本的な方法は`+`を使うことです。ES6以降ではバックティック(``)でテンプレートリテラルを使うともっと読みやすくなります。

```javascript
// "+" 演算子を使用する
let greeting = "こんにちは、" + "世界！";
console.log(greeting); // "こんにちは、世界！"

// テンプレートリテラルを使用する
let name = "太郎";
let age = 25;
let introduction = `私の名前は${name}です。年齢は${age}歳です。`;
console.log(introduction); // "私の名前は太郎です。年齢は25歳です。"
```

## Deep Dive (深掘り)
初期のJavaScriptでは`+`演算子しかなく、長い文字列を連結するのは少々面倒でした。ES6の導入でテンプレートリテラルが加わり、変数の埋め込みが容易になりました。また、`Array.join()` や `concat()` メソッドもありますが、ES6のテンプレートリテラルが最も読みやすく推奨されています。

パフォーマンスについては状況に応じて異なります。`+` 演算子はシンプルな連結には早いですが、多くの文字列を結合する場合にはテンプレートリテラルや`Array.join()`の方が効率的です。古いブラウザサポートが必要な場合、テンプレートリテラルはポリフィル(polyfill)を使う必要があるかもしれません。

## See Also (関連情報)
- MDN Web Docsによるテンプレートリテラルの詳細: [テンプレートリテラル (MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)
- 文字列操作の全般的なガイド: [JavaScript の文字列操作ガイド (MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Text_formatting)
- 異なるJavaScript連結方法のベンチマーク: [JavaScript String Concatenation Benchmarks](https://jsben.ch/)