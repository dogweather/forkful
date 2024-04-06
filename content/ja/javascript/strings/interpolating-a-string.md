---
date: 2024-01-20 17:51:14.625937-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.448371-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
