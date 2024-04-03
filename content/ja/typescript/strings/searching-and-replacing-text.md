---
date: 2024-01-20 17:59:02.610458-07:00
description: "How to: (\u3084\u308A\u65B9) TypeScript\u3067\u306F\u3001`String`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306E`replace()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\
  \u3063\u3066\u30C6\u30AD\u30B9\u30C8\u3092\u691C\u7D22\u3057\u3001\u7F6E\u63DB\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u3092\u898B\u3066\u304F\u3060\u3055\u3044\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.737714-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\u3067\u306F\u3001`String`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306E`replace()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3063\u3066\u30C6\u30AD\u30B9\
  \u30C8\u3092\u691C\u7D22\u3057\u3001\u7F6E\u63DB\u3057\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306E\u4F8B\u3092\u898B\u3066\u304F\u3060\u3055\u3044."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (やり方)
TypeScriptでは、`String`オブジェクトの`replace()`メソッドを使ってテキストを検索し、置換します。以下の例を見てください。

```TypeScript
const greeting: string = "Hello, World!";
const newGreeting: string = greeting.replace("World", "TypeScript");

console.log(newGreeting); // 出力: Hello, TypeScript!
```

`replace()`メソッドは正規表現も受け取れます。一致するすべての文字列を置換するには、グローバルフラグを使います。

```TypeScript
const errors: string = "Error: 001, Error: 002, Error: 003";
const fixedErrors: string = errors.replace(/Error: \d{3}/g, "Fixed");

console.log(fixedErrors); // 出力: Fixed, Fixed, Fixed
```

## Deep Dive (掘り下げ)
歴史的に見ると、テキストの検索と置換はエディタやWordプロセッサで一般的な機能でした。しかしプログラミングにおいても、ログファイルの分析やコードのリファクタリングなどで非常に重要です。

検索置換には`replace()`の他にも方法があります。たとえば、`split()`と`join()`を組み合わせる方法がありますが、通常の操作よりも手間と処理時間がかかるため、単純なテキスト置換では推奨されません。

```TypeScript
const text: string = "apple, banana, apple";
const newText: string = text.split("apple").join("orange");

console.log(newText); // 出力: orange, banana, orange
```

正規表現を使えば、文字列のパターンを柔軟に指定して検出・置換することが可能です。ただし、正規表現は複雑になりがちで、理解するのが難しいこともあります。ですが、マスターすると非常に強力なツールです。

## See Also (関連項目)
- MDN Web DocsのString.prototype.replace() : https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- TypeScriptの公式ドキュメント: https://www.typescriptlang.org/docs/
- 正規表現の入門ガイド: https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions
