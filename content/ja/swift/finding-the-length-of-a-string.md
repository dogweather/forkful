---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Swiftで文字列の長さを求める (Finding the Length of a String in Swift)

## 何とどうして？ (What & Why?)
文字列の長さを求めるとは、文字列が何文字から構成されているかを数えることです。条件分岐やループ処理をする際に、文字列の長さが必要になることがよくあります。

## どうやって：(How to:)

Swiftでは、文字列の長さを取得するためには、`count` プロパティを使用します。以下に例を示します。

```Swift
let str = "こんにちは、世界"
print(str.count) // 結果は "8"
```

上記の例において、"こんにちは、世界"は8文字なので、`print(str.count)`とすると8と表示されます。

## ディープダイブ (Deep Dive)

Swiftでは文字の数を`count`プロパティで簡単に取得できますが、古い言語などではもっと複雑な手法を用いる必要がありました。`count`プロパティの背後では、Stringを文字の配列として扱い、その要素数を数えることで文字列の長さを求めています。

また、`count`以外にも`utf16.count`と`unicodeScalars.count`という方法も存在します。これらは、特定のUnicode文字が2文字として数えられる状況に対応するためのものです。

```Swift
let emoji = "👨‍👩‍👦"
print(emoji.count) // 結果は "1"
print(emoji.utf16.count) // 結果は "8"
```

このケースでは、絵文字は複数のUnicodeスカラー値を持っているため、.countと.utf16.countでは異なる結果になります。

## 参考資料 (See Also)

- Apple公式ドキュメント: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift String Cheat Sheet: [A Cheat Sheet for Swift Strings](https://useyourloaf.com/blog/swift-string-cheat-sheet/)