---
title:                "サブストリングの抽出"
html_title:           "TypeScript: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何かしら？
文字列から一部を取り出すことを文字列の抽出と呼びます。プログラマーたちがこれをする理由は、特定の部分文字列を必要な情報として取り出すためです。

## 方法：
TypeScriptのコードブロック内にあるコーディング例とサンプル出力です。

```TypeScript
const str: string = "Hello World";
const substring: string = str.substring(6);
console.log(substring);

// Output: World
```

```TypeScript
const str: string = "Hello World";
const startIndex: number = 0;
const endIndex: number = 5;
const substring: string = str.substring(startIndex, endIndex);
console.log(substring);

// Output: Hello
```

## 詳しく：
文字列の抽出は、古代のプログラミング言語であるBASICから生まれた概念です。他にも、正規表現や部分文字列の置き換えなどの代替手段があります。抽出方法には、`.substring()`、`.substr()`、`.slice()`のようなメソッドや、インデックス番号を指定することによる手動の抽出方法などがあります。抽出は、文字列操作やデータ処理でよく使用される一般的な機能です。

## 関連リンク：
- [JavaScriptの複数の部分文字列を取得する方法](https://www.w3schools.com/code/tryit.asp?filename=GJAEKFUGAO7W)
- [TypeScriptの文字列操作方法](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#string-slicing)
- [BASIC言語の歴史](https://www.edp.hr/en/bj88_basic_history)