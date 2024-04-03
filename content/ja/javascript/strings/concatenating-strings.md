---
date: 2024-01-20 17:35:07.788160-07:00
description: "How to: (\u65B9\u6CD5) JavaScript\u3067\u6587\u5B57\u5217\u3092\u9023\
  \u7D50\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u306F`+`\u3092\u4F7F\u3046\
  \u3053\u3068\u3067\u3059\u3002ES6\u4EE5\u964D\u3067\u306F\u30D0\u30C3\u30AF\u30C6\
  \u30A3\u30C3\u30AF(``)\u3067\u30C6\u30F3\u30D7\u30EC\u30FC\u30C8\u30EA\u30C6\u30E9\
  \u30EB\u3092\u4F7F\u3046\u3068\u3082\u3063\u3068\u8AAD\u307F\u3084\u3059\u304F\u306A\
  \u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.664637-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\u3067\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\u57FA\u672C\
  \u7684\u306A\u65B9\u6CD5\u306F`+`\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\
  ES6\u4EE5\u964D\u3067\u306F\u30D0\u30C3\u30AF\u30C6\u30A3\u30C3\u30AF(``)\u3067\u30C6\
  \u30F3\u30D7\u30EC\u30FC\u30C8\u30EA\u30C6\u30E9\u30EB\u3092\u4F7F\u3046\u3068\u3082\
  \u3063\u3068\u8AAD\u307F\u3084\u3059\u304F\u306A\u308A\u307E\u3059."
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
