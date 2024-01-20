---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么与为什么?

子字符串提取是从源字符串中复制特定段落生成新的字符串。程序员这么做是为了读取、复制或者修改字符串中的特定部分。

## 如何：
```Swift
let str = "Hello, Swift"
let startIndex = str.index(str.startIndex, offsetBy: 7)
let endIndex = str.index(str.startIndex, offsetBy: 12)
let substring = str[startIndex..<endIndex]
```
在输出：
```Swift
print(substring)
// 输出: "Swift"
```

## 深度挖掘:
* 历史背景: Swift最初没有提供标准的子字符串提取方法，但在Swift 4.0以后，引入了以索引为基础的方法。
* 替代方案: 在Swift中，你也可以使用 NSRange 和 NSString 方式从字符串中提取子字符串。 
* 实现细节: Swift中子字符串的提取实际上是基于源字符串的一个子集视图。这意味着提取操作的执行效率非常高，因为实际并没有进行字符串复制操作。

## 另请参阅:
1. [Swift官方文档: 子字符串](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Substring In Swift](https://learnappmaking.com/substring-swift/)
3. [Working with Strings in Swift](https://www.hackingwithswift.com/articles/175/working-with-strings-in-swift)