---
title:                "文字列の抽出"
html_title:           "Javascript: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ライテングってなぁに？
ライテングとは、文字列から部分文字列を抽出することを指します。プログラマーはライテングを使うことで、長い文字列の一部分だけを取り出したり、必要なデータを取得したりすることができます。

## 使い方
```Javascript
// 文字列の一部分を取得する
let str = "今日の天気は晴れです。";
let weather = str.substring(5, 7);

console.log(weather);
// 出力結果: 天気

// 文字列の長さを取得する
let str = "こんにちは！";
let length = str.length;

console.log(length);
// 出力結果: 6
```

## 詳しく見ていく
ライテングは、古くから使われているテキスト操作の一つです。他の方法として、インデックス参照や正規表現などがありますが、ライテングは簡単に文字列を操作できることで人気があります。Javascriptでは、`substring()`と`length`プロパティを使うことで、ライテングを実現できます。

## 関連情報を見る
- [MDN - substring()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - length](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Codecademy - The Basics of Writing Functions in JavaScript](https://www.codecademy.com/courses/introduction-to-javascript/lessons/functions/exercises/substring)