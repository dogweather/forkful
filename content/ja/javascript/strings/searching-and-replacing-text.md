---
date: 2024-01-20 17:58:03.987490-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u5225\
  \u306E\u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u51E6\u7406\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\
  \u5F62\u3084\u3001\u30B3\u30FC\u30C9\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\
  \u30F3\u306E\u66F4\u65B0\u306A\u3069\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.601266-07:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u5225\
  \u306E\u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u51E6\u7406\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\
  \u5F62\u3084\u3001\u30B3\u30FC\u30C9\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\
  \u30F3\u306E\u66F4\u65B0\u306A\u3069\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
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
