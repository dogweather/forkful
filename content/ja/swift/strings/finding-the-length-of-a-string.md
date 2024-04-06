---
date: 2024-01-20 17:48:36.438276-07:00
description: "How to (\u65B9\u6CD5) Swift\u3067\u306F\u3001`count`\u30D7\u30ED\u30D1\
  \u30C6\u30A3\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u7C21\
  \u5358\u306B\u898B\u3064\u3051\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u3053\u306E\u30D7\u30ED\u30D1\u30C6\u30A3\u306F\u6587\u5B57\u5217\u306B\u542B\u307E\
  \u308C\u308B\u6587\u5B57\u306E\u6570\u3092\u8FD4\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.102638-06:00'
model: gpt-4-1106-preview
summary: "How to (\u65B9\u6CD5) Swift\u3067\u306F\u3001`count`\u30D7\u30ED\u30D1\u30C6\
  \u30A3\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u7C21\u5358\
  \u306B\u898B\u3064\u3051\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u306E\u30D7\u30ED\u30D1\u30C6\u30A3\u306F\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\
  \u308B\u6587\u5B57\u306E\u6570\u3092\u8FD4\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to (方法)
```Swift
let greeting = "こんにちは"
print(greeting.count) // 出力: 5
```

Swiftでは、`count`プロパティを使って文字列の長さを簡単に見つけることができます。このプロパティは文字列に含まれる文字の数を返します。

## Deep Dive (深掘り)
Swiftでは、文字列は`String`型で表され、Unicodeスカラーのコレクションです。これは、文字列内の各文字がどんな言語でも正しく表現されることを意味します。

過去に、プログラミング言語はしばしば単純なASCII文字に依存していましたが、Swiftは最初から国際的な使用を視野に入れて設計されています。そのため、文字列の長さを得るプロセスは、それが絵文字や複合文字クラスターを含むかによらず、確実です。

選択肢として、`characters`プロパティを使っても長さが得られますが、Swift 4からは非推奨となり、`count`が推奨されています。

実装の詳細としては、`count`を呼び出すと、Swiftの内部で文字列を一度走査して、実際の文字カウントを計算します。以下のように多言語のサポートを考慮した複雑なシナリオでも、Swiftは正確な文字数を返します。

```Swift
let complexGreeting = "Hello, 世界🌏!"
print(complexGreeting.count) // 出力: 11
```

上の例では、英語の文字、日本語の文字、そして絵文字が含まれますが、`count`は文字列の長さを正確に11として計算します。

## See Also (関連項目)
- Swift公式ドキュメント：[Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicodeについての深い理解：[Unicode Consortium](https://home.unicode.org/)
- Swiftにおける文字列操作のパフォーマンス：[Swift String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)
