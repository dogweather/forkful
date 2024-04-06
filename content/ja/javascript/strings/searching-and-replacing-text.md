---
date: 2024-01-20 17:58:03.987490-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.447350-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\u3067\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u7F6E\u63DB\u3092\u884C\
  \u3046\u969B\u306B\u3001\u901A\u5E38\u306F`.replace()`\u30E1\u30BD\u30C3\u30C9\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306FECMAScript\u6A19\u6E96\u306E\
  \u4E00\u90E8\u3067\u3042\u308A\u3001\u904E\u53BB\u306EJavaScript\u306E\u30D0\u30FC\
  \u30B8\u30E7\u30F3\u304B\u3089\u5B58\u5728\u3057\u3066\u3044\u307E\u3059\u3002\u5E02\
  \u8CA9\u3055\u308C\u308B\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E2D\u3067\
  \u306F\u3001Lodash\u306A\u3069\u306E\u4EE3\u66FF\u54C1\u3082\u3042\u308A\u307E\u3059\
  \u304C\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u7528\u9014\u3067\u306F`.replace()`\u304C\
  \u901A\u5E38\u6700\u3082\u52B9\u7387\u7684\u3067\u3059\u3002`.replace()`\u306F\u7B2C\
  \u4E00\u5F15\u6570\u306B\u6587\u5B57\u5217\u304B\u6B63\u898F\u8868\u73FE\u3092\u53D6\
  \u308A\u3001\u7B2C\u4E8C\u5F15\u6570\u306B\u7F6E\u63DB\u3059\u308B\u6587\u5B57\u5217\
  \u3092\u53D6\u308B\u3053\u3068\u3067\u52D5\u304D\u307E\u3059\u3002\u6B63\u898F\u8868\
  \u73FE\u3092\u4F7F\u3046\u3053\u3068\u3067\u3001\u6587\u5B57\u5217\u306E\u30D1\u30BF\
  \u30FC\u30F3\u3092\u67D4\u8EDF\u306B\u6307\u5B9A\u3057\u3001`g`\u30D5\u30E9\u30B0\
  \u3092\u4ED8\u3051\u308B\u3053\u3068\u306B\u3088\u3063\u3066\u3001\u5168\u3066\u306E\
  \u4E00\u81F4\u7B87\u6240\u3092\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u304C\u53EF\
  \u80FD\u306B\u306A\u308A\u307E\u3059\u3002"
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
