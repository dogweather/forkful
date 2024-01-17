---
title:                "文字列の長さを見つける"
html_title:           "Javascript: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何をやることか？
文字列の長さを求めることは、プログラマーが文字列を処理する際に非常に重要なことです。文字列の長さを知ることで、特定の操作や処理を行うことができます。プログラマーは、文字列の長さを求めることによって、データを効率的に処理することができます。

## やり方：
```Javascript
// 文字列の長さを求める
let str = "こんにちは！"
let strLength = str.length;
console.log(strLength); // 出力結果：5
```

```Javascript
// 空の文字列の長さを求める
let emptyStr = "";
let emptyStrLength = emptyStr.length;
console.log(emptyStrLength); // 出力結果：0
```

## 詳しく見る：
文字列の長さを求めることは、古くからプログラミングの基本的な操作の一つとして挙げられてきました。この操作は、文字列を使用する多くの処理やアルゴリズムにおいて欠かせないものです。また、現代のプログラミング言語では、文字列の長さを求める方法が標準化されており、容易に実装することができます。しかし、古いプログラミング言語では、文字列の長さを求めるために独自の方法が必要でした。

代替手段として、正規表現を使用することもできます。しかし、正規表現は文字列の全体のパターンマッチングを行うため、文字列の長さを求めることに比べて処理が重く、効率的ではありません。

実装の詳細について言えば、多くのプログラミング言語では、文字列の長さを求めるために、文字列オブジェクトが持つlengthメソッドを使用します。このメソッドは、文字列の長さを整数値で返します。

## 関連リンク：
- [JavaScript 文字列の長さを取得する(length)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [正規表現とは -わかりやすく解説!](https://techacademy.jp/magazine/23328)
- [JavaScriptのlengthメソッドの仕組みを解説](https://reboooot.net/js-length-method/)