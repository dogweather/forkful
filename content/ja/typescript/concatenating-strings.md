---
title:                "文字列の連結"
date:                  2024-01-20T17:35:40.480007-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
文字列の連結は、複数の文字列を結合して一つの文字列を作ることです。プログラマはデータを整形したり、UIに表示する文言を作成するために連結を使います。

## How to: (方法)
```TypeScript
// 簡単な連結
let greeting = "こんにちは、" + "世界！";
console.log(greeting); // "こんにちは、世界！"

// テンプレートリテラルを利用
let user = "太郎";
let message = `こんにちは、${user}さん！`;
console.log(message); // "こんにちは、太郎さん！"
```

## Deep Dive (深掘り)
文字列の連結はプログラミング言語が生まれた当初から存在します。早い時代から重要な機能でした。例えば、C言語では`strcat`関数を利用します。

TypeScriptでは、`+` 演算子やテンプレートリテラルを使って連結できます。`+` はシンプルですが、多くの変数や長い文字列を扱う場合はテンプレートリテラルが読みやすくて便利です。

内部的には、文字列を連結するときに新しい文字列が作成されます。JavaScriptエンジンによっては、多くの小さな連結がパフォーマンスの問題を引き起こす場合があります。その場合は、配列を使用して文字列を組み立て、最終的に`.join('')`メソッドで連結するのがより効率的です。

また、TypeScriptはES6の機能をフルにサポートしており、テンプレートリテラルはES6で導入された機能です。これにより複数行の文字列や式の埋め込みが容易になりました。

## See Also (関連情報)
- TypeScriptの公式ドキュメント: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- MDN Web Docsの文字列ページ: [String - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- ES6テンプレートリテラル: [Template literals (Template strings)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)
