---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JavaScriptでは、正規表現（Regex）を使って、文字列内のパターンを検索・置換します。プログラマは、データのバリデーション、抽出、置換など効率的に行うために正規表現を利用します。

## How to: (方法)
```javascript
// 文字列内でのパターンマッチ
const greeting = 'こんにちは、世界！';
const regex = /こんにちは/;
const found = regex.test(greeting);
console.log(found); // 出力: true

// メールアドレスのバリデーション
const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const email = 'example@mail.com';
const isValidEmail = emailRegex.test(email);
console.log(isValidEmail); // 出力: true

// グローバル置換
const messyString = 'Apples are round, and apples are juicy.';
const fixedString = messyString.replace(/apples/gi, "oranges");
console.log(fixedString); // 出力: Oranges are round, and oranges are juicy.
```

## Deep Dive (深掘り)
正規表現は、1960年代に発明されUnixなどで広く使われてきた。JavaScriptの他、多くのプログラミング言語やテキスト処理ツールもサポートしています。正規表現は強力ですが、複雑なパターンは読みづらく、遅くなることも。そのため、時と場合によっては、文字列のメソッド（`indexOf`, `startsWith`など）やパーサライブラリを使う方が良い場合もあります。

## See Also (関連情報)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExpオブジェクト - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [JavaScript.info: Regular expressions](https://javascript.info/regular-expressions)