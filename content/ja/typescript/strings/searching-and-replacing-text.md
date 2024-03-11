---
date: 2024-01-20 17:59:02.610458-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u51FA\u3057\
  \u3066\u3001\u305D\u308C\u3092\u4ED6\u306E\u6587\u5B57\u5217\u306B\u5909\u3048\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\
  \u3092\u6574\u7406\u3057\u305F\u308A\u3001\u30B3\u30FC\u30C9\u5185\u306E\u8AA4\u5B57\
  \u3092\u4FEE\u6B63\u3057\u305F\u308A\u3059\u308B\u6642\u306B\u4F7F\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.338094-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u51FA\u3057\
  \u3066\u3001\u305D\u308C\u3092\u4ED6\u306E\u6587\u5B57\u5217\u306B\u5909\u3048\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\
  \u3092\u6574\u7406\u3057\u305F\u308A\u3001\u30B3\u30FC\u30C9\u5185\u306E\u8AA4\u5B57\
  \u3092\u4FEE\u6B63\u3057\u305F\u308A\u3059\u308B\u6642\u306B\u4F7F\u3044\u307E\u3059\
  \u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
テキストの検索と置換は、指定された文字列を見つけ出して、それを他の文字列に変えることです。プログラマはデータを整理したり、コード内の誤字を修正したりする時に使います。

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
