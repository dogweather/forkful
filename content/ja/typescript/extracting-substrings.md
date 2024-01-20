---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なんでしょうか？そして、なぜ？

部分文字列の抽出は、元の文字列から特定の範囲の文字を取り出すプロセスのことを指します。開発者は主にデータ操作や情報のフィルタリングを実行するために、この技術を使用します。

## 使い方：

TypeScriptでは、「substring」や「slice」メソッドで部分文字列を抽出します。

```TypeScript
let str = 'TypeScript サブストリング';
console.log(str.substring(0,10));   // Output: 'TypeScript'
console.log(str.slice(start));       // Output: 'サブストリング'
```

## ディープダイブ：

部分文字列の抽出は、ユーザからの入力が予測不能で多様性のあるシチュエーションや大量のテキストデータの解析など、多くのプログラミング場面で使用されます。

伝統的には、「substring」メソッドは開始位置と終了位置を受け取り、「slice」メソッドは開始位置と抽出する文字数を受け取りますが、TypeScriptでは両メソッドとも同じ挙動をします。

なお、他の抽出の方法として「substr」メソッドも存在しますが、現在は非推奨とされ、確実な互換性のためには「substring」か「slice」メソッドを利用するべきです。

## その他参考になる情報源：

TypeScript公式ドキュメントの該当部分:
- [`String.prototype.substring()`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [`String.prototype.slice()`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)

これらのリンクは、TypeScriptでの部分文字列の抽出についてより深く学ぶための参考資料となります。