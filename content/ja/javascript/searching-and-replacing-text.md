---
title:                "テキストの検索と置換"
html_title:           "Javascript: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何&なぜ？

検索と置換テキストとは、コンピュータープログラムであるテキストを探し、特定のテキストで置き換えることを指します。プログラマーは、特定のテキストを大量に置き換えたり、複数のファイルで同じ変更を行う必要があるため、これを行います。

## 方法：
```
// テキストを置換する例
let str = "Hello world!";
let newStr = str.replace("world", "universe");
console.log(newStr);
// 出力：Hello universe!
```

```
// 正規表現を使用して複数のテキストを置換する例
let str = "I have 3 apples and 5 oranges.";
let newStr = str.replace(/\d+/g, "2");
console.log(newStr);
// 出力：I have 2 apples and 2 oranges.
```

## 深い探求:

検索と置換は、コンピューターが発明された初期から存在していた技術です。最も一般的な方法は、文字列を探索するためのループを使っていましたが、現代のプログラミング言語では効率的なメソッドや正規表現を使用できます。代替手段としては、コマンドラインツールやテキストエディターの置換機能もあります。実装に関する詳細情報は、個々の言語や環境のドキュメントを参照してください。

## 参考文献：

- [MDN web docs: テキストを置換する](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [正規表現チュートリアル](https://www.w3schools.com/jsref/jsref_replace.asp)
- [テキストエディターの機能を活用する - 検索と置換](https://canvasry.com/web-studio/srch-rplc-slt.html)