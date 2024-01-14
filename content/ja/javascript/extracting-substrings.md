---
title:                "Javascript: 部分文字列の抽出"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
サブストリングを抽出する理由を説明するためには、文字列の特定の部分を取得したい場合があるからです。たとえば、あなたが作業しているテキストエディタが特定の言語の文法ハイライトをサポートしている場合、その単語の文字列を取得して、特定の色やスタイルを適用する必要があります。これは、一般的なプログラムの特定の部分から情報を抽出する必要がある場合も同様です。

## 方法
サブストリングを抽出する方法についてはいくつかのオプションがあります。最も一般的な方法の1つは、標準のJavaScriptの```substring ()```メソッドを使用することです。このメソッドは、指定された開始位置と終了位置の間で、元の文字列から部分文字列を切り出します。例えば、以下のコードを使用して、"hello"文字列の最初の3文字を抽出することができます。

```Javascript
let str = "hello";
let sub = str.substring(0, 3); // "hel"
console.log(sub); // "hel"
```

また、```slice ()```メソッドも同様の機能を提供します。このメソッドでは、負のインデックスを使用することで、末尾からの文字列の抽出も可能です。詳細な使い方については、公式のドキュメントを参照してください。

## ディープダイブ
サブストリング抽出のさまざまな方法を調べる際には、文字列を操作する様々な方法を学ぶことが重要です。例えば、正規表現を使用してマッチングパターンを作成し、特定の文字列の部分を抽出することができます。また、特定の文字列を分割して、部分文字列の配列を作成することもできます。

さらに、サブストリングをより高度に取得する必要がある場合は、文字列操作ライブラリを使用することもできます。これらのライブラリには、さまざまな便利なメソッドが用意されており、より複雑な操作を行うことができます。例えば、LodashやStrmanなどがあります。

## See Also
- [JavaScriptのsubstring（）メソッドの詳細について](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [JavaScriptのslice()メソッドの詳細について](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [正規表現の基本について学ぶ](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [より高度な文字列操作を行うためのライブラリ](https://github.com/lodash/lodash)
- [サブストリングの使用例と使い方ガイド](https://www.w3schools.com/jsref/jsref_substr.asp)