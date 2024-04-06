---
date: 2024-01-20 17:38:47.081462-07:00
description: "How to: (\u65B9\u6CD5) JavaScript\u3067\u6587\u5B57\u5217\u3092\u5C0F\
  \u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u306B\u306F\u3001`toLowerCase()` \u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u30B3\u30FC\
  \u30C9\u4F8B\u3068\u51FA\u529B\u30B5\u30F3\u30D7\u30EB\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.449399-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) JavaScript\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u306B\u306F\u3001`toLowerCase()` \u30E1\u30BD\u30C3\
  \u30C9\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u30B3\u30FC\u30C9\u4F8B\
  \u3068\u51FA\u529B\u30B5\u30F3\u30D7\u30EB\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## How to: (方法)
JavaScriptで文字列を小文字に変換するには、`toLowerCase()` メソッドを使います。以下はコード例と出力サンプルです。

```javascript
let greeting = "こんにちは、WORLD!";
let lowerCaseGreeting = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "こんにちは、world!"
```

## Deep Dive (掘り下げ)
JavaScriptが最初に登場した頃から、文字列操作は基本的な機能の1つです。`toLowerCase()` メソッドは、ECMAScript標準の初期版において定義されており、今日に至るまで広く使われています。

代替手段として、正規表現と `replace()` メソッドを組み合わせることもできますが、これは非効率であり、単純な小文字変換には `toLowerCase()` の方が適しています。

小文字変換の実装はブラウザやJavaScriptエンジンによって異なりますが、Unicode標準に従い、さまざまな言語や特殊文字に対応していることが期待されます。

## See Also (関連情報)
- MDN Web Docsの `toLowerCase()` の解説: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- ECMAScript標準の最新版: https://www.ecma-international.org/ecma-262/
