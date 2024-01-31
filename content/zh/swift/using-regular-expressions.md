---
title:                "使用正则表达式"
date:                  2024-01-19
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么?)
正则表达式用于匹配字符串模式。程序员使用它们进行搜索、替换、数据验证等，提高代码效率和灵活性。

## How to: (如何操作:)
```Swift
import Foundation

let string = "My email is me@example.com"
if let range = string.range(of: "[a-zA-Z0-9.]+@[a-zA-Z0-9.]+\\.[a-zA-Z]{2,}", options: .regularExpression) {
    let email = string[range]
    print(email) // 输出: me@example.com
}
```

```Swift
let text = "The year 2023 looks promising."
let regex = try! NSRegularExpression(pattern: "\\b\\d{4}\\b")
let results = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
let allYears = results.map { String(text[Range($0.range, in: text)!]) }
print(allYears) // 输出: ["2023"]
```

## Deep Dive (深入探索):
1. 历史背景: Perl是第一个广泛使用正则表达式的语言，后来许多语言都借鉴了这个特性。
2. 替代选项: 对于简单的模式匹配，可以使用字符串方法，如`contains`, `hasPrefix`等。
3. 实现细节: Swift内部使用`NSRegularExpression`类，这是基于Objective-C的Foundation框架的。

## See Also (参考链接):
- [Apple Developer Documentation - NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift.org - String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [raywenderlich.com - Swift Regex Tutorial](https://www.raywenderlich.com/2295-regular-expressions-tutorial-getting-started)
