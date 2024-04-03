---
date: 2024-01-20 17:52:10.250916-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.738950-06:00'
model: gpt-4-1106-preview
summary: .
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
