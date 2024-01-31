---
title:                "文字列の補間"
date:                  2024-01-20T17:52:10.250916-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

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
