---
date: 2024-01-20 17:51:14.625937-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u3084\u8A08\u7B97\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u5229\u7528\u3059\u308B\u7406\u7531\
  \u306F\u3001\u52D5\u7684\u306A\u60C5\u5831\u3092\u52B9\u7387\u7684\u306B\u9023\u7D50\
  \u3057\u3001\u53EF\u8AAD\u6027\u306E\u9AD8\u3044\u30B3\u30FC\u30C9\u3092\u66F8\u304F\
  \u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.656466-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u3084\u8A08\u7B97\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u5229\u7528\u3059\u308B\u7406\u7531\
  \u306F\u3001\u52D5\u7684\u306A\u60C5\u5831\u3092\u52B9\u7387\u7684\u306B\u9023\u7D50\
  \u3057\u3001\u53EF\u8AAD\u6027\u306E\u9AD8\u3044\u30B3\u30FC\u30C9\u3092\u66F8\u304F\
  \u305F\u3081\u3067\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## What & Why?
文字列補間とは、文字列の中に変数や計算結果を埋め込むことです。これを利用する理由は、動的な情報を効率的に連結し、可読性の高いコードを書くためです。

## How to:
### 文字列補間の基本
```javascript
const name = "Yoshi";
const message = `こんにちは、${name}さん！`;
console.log(message); // "こんにちは、Yoshiさん！"
```

### 計算の埋め込み
```javascript
const price = 1500;
const taxRate = 0.1;
const total = `合計金額: ${price * (1 + taxRate)}円`;
console.log(total); // "合計金額: 1650円"
```

### 複数の変数の組み合わせ
```javascript
const user = { firstName: "Taro", lastName: "Yamada" };
const greeting = `名前: ${user.firstName} ${user.lastName}`;
console.log(greeting); // "名前: Taro Yamada"
```

## Deep Dive
文字列補間は、ES6（ECMAScript 2015）で追加されたテンプレートリテラルにより容易になりました。それ以前は、'+'演算子を使った文字列の連結が一般的でしたが、テンプレートリテラルはより直感的でエラーの少ないコードを書くことを可能にします。

```javascript
// ES5以前の例
var name = "Yoshi";
var message = "こんにちは、" + name + "さん！";
console.log(message);
```

ただし、文字列補間には注意点もあります。たとえば、ユーザーからの入力をそのまま埋め込むと、セキュリティ上の問題（XSS攻撃など）につながる可能性がありますので注意が必要です。

## See Also
- MDN Web Docs on Template Literals: [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- ES6 Features: [ES6 Features - Template Literals](http://es6-features.org/#StringInterpolation)
- WebサイトやアプリにJavaScriptを組み込む基礎を学びたい場合: [JavaScript for Cats](http://jsforcats.com/)
