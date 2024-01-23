---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:48:36.438276-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列の長さを測るのは、ただその文字の数を数えることです。プログラマーがこれを行う理由は、テキストデータを検証したり、フォーマットを調整したり、ユーザーインターフェイスを管理する時に必要だからです。

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
