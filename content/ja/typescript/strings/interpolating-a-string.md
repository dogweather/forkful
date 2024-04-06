---
date: 2024-01-20 17:52:10.250916-07:00
description: "How to: (\u3084\u308A\u65B9) \u4E0A\u306E\u30B3\u30FC\u30C9\u306F\u30C6\
  \u30F3\u30D7\u30EC\u30FC\u30C8\u30EA\u30C6\u30E9\u30EB\uFF08\u30D0\u30C3\u30AF\u30AF\
  \u30A9\u30FC\u30C8\u3067\u56F2\u3080\uFF09\u3092\u4F7F\u3063\u3066\u3044\u307E\u3059\
  \u3002\u5909\u6570\u3084\u5F0F\u3092`${}`\u3067\u56F2\u3080\u3068\u305D\u306E\u5024\
  \u304C\u6587\u5B57\u5217\u306B\u633F\u5165\u3055\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.655844-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u4E0A\u306E\u30B3\u30FC\u30C9\u306F\u30C6\u30F3\u30D7\
  \u30EC\u30FC\u30C8\u30EA\u30C6\u30E9\u30EB\uFF08\u30D0\u30C3\u30AF\u30AF\u30A9\u30FC\
  \u30C8\u3067\u56F2\u3080\uFF09\u3092\u4F7F\u3063\u3066\u3044\u307E\u3059\u3002\u5909\
  \u6570\u3084\u5F0F\u3092`${}`\u3067\u56F2\u3080\u3068\u305D\u306E\u5024\u304C\u6587\
  \u5B57\u5217\u306B\u633F\u5165\u3055\u308C\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to: (やり方)
```typescript
let user = 'Yamada';
let message = `こんにちは、${user}さん！`;
console.log(message); // "こんにちは、Yamadaさん！"

let price = 1500;
let taxRate = 0.1;
let total = `合計: ${price * (1 + taxRate)}円`;
console.log(total); // "合計: 1650円"
```
上のコードはテンプレートリテラル（バッククォートで囲む）を使っています。変数や式を`${}`で囲むとその値が文字列に挿入されます。

## Deep Dive (探求)
文字列補間はECMAScript 2015 (ES6)で導入されました。それ以前は、プラス記号を使用して文字列を連結する必要がありました。
```typescript
// ES5以前の例
let user = 'Yamada';
let message = 'こんにちは、' + user + 'さん！';
console.log(message);
```
テンプレートリテラルの利点は可読性が高く、複数行の文字列も簡単に扱える点です。また、関数を埋め込むこともできます。
```typescript
function totalPrice(price: number, taxRate: number): string {
  return `合計: ${price * (1 + taxRate)}円`;
}

console.log(totalPrice(1500, 0.1)); // "合計: 1650円"
```
さらに、タグ付きテンプレートリテラルを使えば、補間される値の処理方法をカスタマイズすることもできます。

## See Also (関連する情報)
- [Template literals (Template strings)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) - MDN Web Docs
- [TypeScript Handbook: Template Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html) - 公式TypeScriptハンドブック
