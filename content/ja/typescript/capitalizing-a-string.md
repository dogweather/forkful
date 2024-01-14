---
title:                "TypeScript: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
文字列を大文字化するのは、多くのプログラミング言語でよく使用される単純なタスクです。ただし、TypeScriptを使用すると、変数や関数などの複雑なデータ型を扱う際にも文字列を大文字化する必要がある場合があります。

## How To
```TypeScript
// 文字列を大文字化する関数の定義
function capitalize(str: string) {
  // 変数に大文字化した文字列を格納する
  let capStr: string = str.toUpperCase();
  // 変数を返す
  return capStr;
}

// 関数の使用例
let name: string = "taro";
console.log(capitalize(name));

// 出力結果: "TARO"
```

## Deep Dive
文字列を大文字化する方法はいくつかありますが、今回はTypeScriptの`toUpperCase()`メソッドを使用しました。これはJavaScriptでの標準的な方法であり、TypeScriptでもサポートされています。

また、文字列を大文字化する際に使用することができる他のメソッドとして`charAt()`や`slice()`などもあります。これらのメソッドは、文字列の特定の文字を取り出して大文字化したり、部分的に大文字化することもできます。

## See Also
- [TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/)
- [文字列を操作するためのJavaScriptメソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)