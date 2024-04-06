---
date: 2024-01-20 17:35:40.480007-07:00
description: "How to: (\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u304C\u751F\u307E\u308C\u305F\u5F53\
  \u521D\u304B\u3089\u5B58\u5728\u3057\u307E\u3059\u3002\u65E9\u3044\u6642\u4EE3\u304B\
  \u3089\u91CD\u8981\u306A\u6A5F\u80FD\u3067\u3057\u305F\u3002\u4F8B\u3048\u3070\u3001\
  C\u8A00\u8A9E\u3067\u306F`strcat`\u95A2\u6570\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002 TypeScript\u3067\u306F\u3001`+` \u6F14\u7B97\u5B50\u3084\u30C6\u30F3\u30D7\
  \u30EC\u30FC\u30C8\u30EA\u30C6\u30E9\u30EB\u3092\u4F7F\u3063\u3066\u9023\u7D50\u3067\
  \u304D\u307E\u3059\u3002`+`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.662459-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u304C\u751F\u307E\u308C\u305F\u5F53\u521D\u304B\
  \u3089\u5B58\u5728\u3057\u307E\u3059\u3002\u65E9\u3044\u6642\u4EE3\u304B\u3089\u91CD\
  \u8981\u306A\u6A5F\u80FD\u3067\u3057\u305F\u3002\u4F8B\u3048\u3070\u3001C\u8A00\u8A9E\
  \u3067\u306F`strcat`\u95A2\u6570\u3092\u5229\u7528\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
```TypeScript
// 簡単な連結
let greeting = "こんにちは、" + "世界！";
console.log(greeting); // "こんにちは、世界！"

// テンプレートリテラルを利用
let user = "太郎";
let message = `こんにちは、${user}さん！`;
console.log(message); // "こんにちは、太郎さん！"
```

## Deep Dive (深掘り)
文字列の連結はプログラミング言語が生まれた当初から存在します。早い時代から重要な機能でした。例えば、C言語では`strcat`関数を利用します。

TypeScriptでは、`+` 演算子やテンプレートリテラルを使って連結できます。`+` はシンプルですが、多くの変数や長い文字列を扱う場合はテンプレートリテラルが読みやすくて便利です。

内部的には、文字列を連結するときに新しい文字列が作成されます。JavaScriptエンジンによっては、多くの小さな連結がパフォーマンスの問題を引き起こす場合があります。その場合は、配列を使用して文字列を組み立て、最終的に`.join('')`メソッドで連結するのがより効率的です。

また、TypeScriptはES6の機能をフルにサポートしており、テンプレートリテラルはES6で導入された機能です。これにより複数行の文字列や式の埋め込みが容易になりました。

## See Also (関連情報)
- TypeScriptの公式ドキュメント: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- MDN Web Docsの文字列ページ: [String - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- ES6テンプレートリテラル: [Template literals (Template strings)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)
