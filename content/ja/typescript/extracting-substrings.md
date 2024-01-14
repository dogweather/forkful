---
title:                "TypeScript: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ抽出サブストリングを行うのか

抽出サブストリングを行うことには、テキストデータや文字列をより効率的に処理するためのメリットがあります。例えば、大規模な文字列から特定の部分だけを取り出したいときや、文字列内の特定のキーワードを抽出するといった場合に有用です。

## 方法

抽出サブストリングを行うためには、TypeScriptの組み込みのメソッドである`substring()`を使用します。このメソッドは指定した範囲の文字列を抽出し、新しい文字列として返します。以下は、このメソッドを使用した抽出サブストリングのコード例です。

```TypeScript
// 文字列を宣言する
let str: string = "今日はいい天気です。"

// substring()メソッドを使用して文字列の一部を抽出する
let subStr: string = str.substring(3, 6)

// 抽出した文字列を出力する
console.log(subStr)  // 出力結果: いい

```

## ディープダイブ

`substring()`メソッドは、指定した位置の文字から指定した範囲の文字列を抽出することができます。また、開始位置を省略することで、指定した位置から最後までの文字列を抽出することもできます。さらに、マイナス値を指定することで、後ろから数えた文字列を抽出することも可能です。詳細な使用方法については、公式ドキュメントを参照してください。

## See Also

- [substring()メソッドの公式ドキュメント (英語)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [TypeScript 公式ドキュメント (日本語)](https://www.typescriptlang.org/ja/docs/home.html)
- [抽出サブストリングの例を含むTypeScriptのチュートリアル (日本語)](https://www.w3schools.com/jsref/jsref_substring.asp)