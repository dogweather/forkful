---
title:                "文字列の補間"
date:                  2024-01-20T17:51:14.625937-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

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
