---
title:                "パターンにマッチする文字を削除する"
html_title:           "Swift: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
文字列から特定のパターンにマッチする文字を削除することに興味があるのか、最大2文で説明します。

## 方法
文字列から特定のパターンにマッチする文字を削除する方法を説明します。下記のコーディング例と出力結果を確認してください。

```Swift
// 例1: "Hello, Swift!"からスペースを削除する
let string = "Hello, Swift!"
let result = string.replacingOccurrences(of: " ", with: "")
print(result) // 出力結果: "Hello,Swift!"

// 例2: "apple, orange, banana"からカンマを削除する
let string = "apple,orange,banana"
let result = string.components(separatedBy: ",").joined()
print(result) // 出力結果: "appleorangebanana"
```

## 深堀り
文字列の検索や置換が可能な`replacingOccurrences`や`components(separatedBy:)`を使用することで、特定のパターンにマッチする文字を削除することができます。パターンを指定する際には正規表現を使用することもできます。

## 参考リンク
- [String](https://developer.apple.com/documentation/foundation/string)
- [Regular expressions in Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
- [String.replaceAll(regex, replacement)](https://www.swiftdevcenter.com/string-replaceall-regex-replacement-swift/)