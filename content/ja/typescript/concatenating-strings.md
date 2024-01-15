---
title:                "文字列の連結"
html_title:           "TypeScript: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を結合するのに文を最大2文で説明する。

文字列の結合は、プログラムで異なる文字列を組み合わせる必要がある時に非常に便利です。例えば、フルネームを作るためには、名前と姓を結合することができます。また、メッセージを作成する際には、固定のテキストと動的に変わる変数を一緒に表示することができます。これにより、コードを簡潔に保ち、読みやすくすることができます。

## 作法

```TypeScript
// 単純な文字列の結合
let firstName: string = "太郎";
let lastName: string = "山田";
let fullName: string = firstName + lastName;

console.log(fullName); // 出力: 太郎山田

// 文字列と数字の結合
let age: number = 25;
let message: string = "私は" + age + "歳です。";

console.log(message); // 出力: 私は25歳です。

// テンプレート文字列を使用した結合
let fruit: string = "りんご";
let quantity: number = 3;

let order: string = `私は${fruit}を${quantity}個注文したいです。`;

console.log(order); // 出力: 私はりんごを3個注文したいです。
```

## 深掘り

文字列の結合には、2つの主要な方法があります。最初は、`+`演算子を使用する方法です。この方法では、異なる型の値を自動的に文字列に変換して結合します。もう一つは、テンプレート文字列を使用する方法です。これは、文字列をより簡単に結合し、可読性を高める方法です。また、テンプレート文字列を使用すると、文字列内にJavaScriptの式を挿入することもできます。

さらに、TypeScriptでは、文字列を結合する際に型安全性を考慮することができます。例えば、`+`演算子を使用すると、異なる型の値を結合してもエラーが発生しません。しかし、文字列と数値を結合する際には、意図しない結果になる可能性があり、バグの原因になることがあります。そのため、文字列と数値を結合する際には、テンプレート文字列を使用するか、明示的に文字列に変換するようにすることが推奨されます。

## 参考

- [TypeScript Handbook - String Concatenation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-concatenation)
- [MDN web Docs - Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)