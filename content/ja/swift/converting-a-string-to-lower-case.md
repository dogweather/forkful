---
title:                "Swift: 小文字に文字列を変換する"
simple_title:         "小文字に文字列を変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜ

文字列を小文字に変換することに関心があるかもしれません。例えば、ユーザーからの入力を標準化したい場合や、文字列の比較を行う必要がある場合などが考えられます。

# 方法

文字列を小文字に変換する方法は簡単です。 `lowercased()` メソッドを使用するだけです。以下のように ```Swift ... ``` コードブロック内で例を示します。

```Swift
let str = "Hello, World!"
let lowercasedStr = str.lowercased()
print(lowercasedStr) // 出力: hello, world!
```

# 深堀り

文字列は Swift で不変なので、`lowercased()` メソッドを使用すると新しい文字列が作成されます。また、ロケールに応じて文字を正しく小文字に変換するためには、`lowercased()` メソッドの引数として `Locale` オブジェクトを渡すこともできます。

```Swift
let str = "Äpfel"
let germanLocale = Locale(identifier: "de_DE")
let lowercasedStr = str.lowercased(with: germanLocale)
print(lowercasedStr) // 出力: äpfel
```

しかし、文字列を小文字に変換するだけでなく、使用できるメソッドはさらにたくさんあります。例えば、`uppercased()` メソッドを使用すると文字列を大文字に変換できます。詳細は[ドキュメント](https://developer.apple.com/documentation/foundation/nslocalizedstring/1408241-lowercased)を参照してください。

# 参考

- [Strings and Characters - The Swift Programming Language](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String - Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)