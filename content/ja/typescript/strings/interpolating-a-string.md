---
date: 2024-01-20 17:52:10.250916-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u3084\u8A08\u7B97\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\
  \u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\
  \u7684\u306A\u30C7\u30FC\u30BF\u3092\u4F7F\u3046\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  SQL\u306E\u30AF\u30A8\u30EA\u306A\u3069\u3092\u67D4\u8EDF\u306B\u4F5C\u6210\u3059\
  \u308B\u3053\u3068\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.738950-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u5909\u6570\u3084\u8A08\u7B97\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\
  \u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\
  \u7684\u306A\u30C7\u30FC\u30BF\u3092\u4F7F\u3046\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  SQL\u306E\u30AF\u30A8\u30EA\u306A\u3069\u3092\u67D4\u8EDF\u306B\u4F5C\u6210\u3059\
  \u308B\u3053\u3068\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## What & Why? (何となぜ？)
文字列補間とは、文字列の中に変数や計算式の結果を埋め込むことです。これにより、動的なデータを使うメッセージやSQLのクエリなどを柔軟に作成することが可能になります。

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
