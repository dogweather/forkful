---
date: 2024-01-20 17:58:03.987490-07:00
description: 'How to: .'
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.655309-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
