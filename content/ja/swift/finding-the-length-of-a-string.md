---
title:    "Swift: 文字列の長さを見つける"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの重要性についてです。

## 方法

文字列の長さを求めるには、```Swift 
.count ```メソッドを使用します。例えば、```Swift
var name = "Apple" ```という文字列があった場合、```Swift
name.count ```を実行すると、結果は ```Swift4 ```となります。

## 深堀り

このメソッドは、文字列の長さを数えるだけでなく、スペースや句読点などの区切り文字を含めて数えることもできます。また、マルチバイト文字や絵文字を含む文字列でも正しい長さを取得することができます。

## 併せて参照してください

- [Apple Developer Documentation - String.count](https://developer.apple.com/documentation/swift/string/2894566-count)
- [Swift by Sundell - Finding the length of a string in Swift](https://www.swiftbysundell.com/articles/finding-the-length-of-a-string-in-swift/)