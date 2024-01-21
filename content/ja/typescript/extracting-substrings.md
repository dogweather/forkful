---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:46:53.874955-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
文字列の一部を取り出すことを「サブストリングを抽出する」と言います。データの一部を切り取り、表示したり処理したりするためにプログラマーはこれをよく行います。

## How to: (やり方)
```TypeScript
// 文字列から部分文字列を取得する方法

const fullText: string = 'こんにちは、TypeScript!';

// substringメソッド
const sub1: string = fullText.substring(0, 5);
console.log(sub1);  // 出力: こんにちは

// sliceメソッド
const sub2: string = fullText.slice(7);
console.log(sub2);  // 出力: TypeScript!

// substrメソッド (非推奨、使わない方が良い)
const sub3: string = fullText.substr(7, 10);
console.log(sub3);  // 出力: TypeScript!
```

## Deep Dive (深掘り)
最初はJavaScriptからサブストリング抽出メソッドを受け継いだTypeScript。`substring`, `slice`, `substr`の3つのメソッドがあります。ただし、`substr`は非推奨で将来のバージョンで削除される可能性があります。新しいコードでは`substring`か`slice`を使いましょう。

`substring`と`slice`の違いは、主に引数に負の値を指定できるかどうかです。`slice`は負の値を使って後ろから文字を数えられますが、`substring`ではできません。また、`substring`メソッドは引数の順番が逆でも最小値を始点、最大値を終点と解釈します。

他の言語でも似たような関数があり、そこでの経験がTypeScriptでの理解に繋がるでしょう。

## See Also (関連情報)
- TypeScript公式ドキュメント: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- MDN Web Docs (文字列): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- String.prototype.slice() vs String.prototype.substring(): [https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring)