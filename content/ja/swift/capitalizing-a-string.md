---
title:    "Swift: ストリングを大文字に変換する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ

文字列の大文字化に取り組む必要があるのかを説明するためのたった1~2文。

## 使い方

"```Swift
var str = \"hello, world\"
str = str.uppercased()
print(str)
```"
 
上記のように、文字列を大文字化するには、文字列変数に対して`uppercased()`メソッドを使用します。もしくは、`capitalized`プロパティを使用して単語の先頭文字のみを大文字にすることもできます。

出力:
HELLO, WORLD

## ディープダイブ

文字列を大文字化する方法の詳細については、Swiftのドキュメンテーションをご覧ください。また、文字列内の特定の文字を大文字化する方法や、ローケライズを考慮した大文字化など、さまざまなオプションがあります。

## 参考リンク

- [Swiftのドキュメンテーション](https://developer.apple.com/documentation/swift/string)
- [文字列操作についてのSwiftドキュメンテーション](https://developer.apple.com/documentation/swift/string/from_strings_to_characters)
- [文字列の大文字化についての記事](https://www.hackingwithswift.com/example-code/strings/how-to-capitalise-the-first-letter-of-a-string)