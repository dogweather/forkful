---
date: 2024-01-20 17:35:07.788160-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u4E8C\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u3066\u4E00\u3064\u306B\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u3092\u7D44\u307F\u5408\u308F\u305B\u305F\u308A\u3001\u30E6\u30FC\u30B6\
  \u30FC\u306B\u8868\u793A\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.664637-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u4E8C\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u3066\u4E00\u3064\u306B\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u3092\u7D44\u307F\u5408\u308F\u305B\u305F\u308A\u3001\u30E6\u30FC\u30B6\
  \u30FC\u306B\u8868\u793A\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
