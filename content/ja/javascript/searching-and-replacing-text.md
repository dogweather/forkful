---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:03.987490-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
テキストの検索と置換は、指定された文字列を見つけて別の文字列で置き換える処理です。プログラマーは、データの整形や、コード内の特定のパターンの更新などを行うためにこれを行います。

## How to:
```javascript
// 文字列の置換

const phrase = "こんにちは、世界！こんにちは、宇宙！";
const newPhrase = phrase.replace("こんにちは", "さようなら");

console.log(newPhrase); // 出力: "さようなら、世界！こんにちは、宇宙！"

// グローバル置換
const globalNewPhrase = phrase.replace(/こんにちは/g, "さようなら");

console.log(globalNewPhrase); // 出力: "さようなら、世界！さようなら、宇宙！"
```

## Deep Dive
JavaScriptでテキスト検索置換を行う際に、通常は`.replace()`メソッドを使用します。これはECMAScript標準の一部であり、過去のJavaScriptのバージョンから存在しています。市販される標準ライブラリの中では、Lodashなどの代替品もありますが、シンプルな用途では`.replace()`が通常最も効率的です。`.replace()`は第一引数に文字列か正規表現を取り、第二引数に置換する文字列を取ることで動きます。正規表現を使うことで、文字列のパターンを柔軟に指定し、`g`フラグを付けることによって、全ての一致箇所を置き換えることが可能になります。

## See Also
- MDN Web Docs の `.replace()`方法: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- JavaScript 正規表現ガイド: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- Lodash 文字列処理関数: https://lodash.com/docs/#replace
