---
title:                "Swift: 部分文字列を抽出する"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

文字列の一部の部分を取り出すことは、Swiftプログラミングで非常に便利です。例えば、ユーザーから入力された文字列から特定のキーワードを抽出したり、文字列をフォーマットしたりする際に役立つことがあります。

## How To

```Swift
let str = "今はSwiftを勉強しています。"

// "Swift"という文字列を取り出す
let substring1 = str[2...6]
print(substring1) // Output: Swift

// "を"以降の文字列を取り出す
let substring2 = str[6...]
print(substring2) // Output: を勉強しています。

// "Swiftを勉強しています。"という部分を"Swiftを学ぶ"に変更する
var newStr = str
newStr.replaceSubrange(6..., with: "を学ぶ")
print(newStr) // Output: 今はSwiftを学ぶ

```

## Deep Dive

Swiftでは、文字列型に組み込まれている`subscript`機能を使うことで、文字列の一部を取り出すことができます。`subscript`を使用する際は、文字列のインデックス(開始位置)を指定し、`subscript`の引数に開始位置から取り出したい文字の数を指定します。また、範囲演算子`...`を使用することで、開始位置から終了位置までの文字列を取り出すことができます。さらに、`replaceSubrange`メソッドを使用することで、指定した範囲の文字列を別の文字列で置き換えることもできます。

## See Also

- [文字列型 - Swift公式ドキュメント](https://developer.apple.com/documentation/swift/string)
- [String.SubSequence - Swift公式ドキュメント](https://developer.apple.com/documentation/swift/string/subsequence)
- [String の繰り返しや他の方法を使ってサブ文字列にアクセスする - Swift公式ドキュメント](https://developer.apple.com/documentation/swift/string/2994856-split)