---
title:                "字符串首字母大写"
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串大写就是将所有字符转换为大写形式。程序员这样做主要是为了统一格式，例如让标题或用户输入更加规范。

## How to: (如何操作：)
Swift 提供了简单的方法来将字符串变成大写。使用 `uppercased()` 函数就可以了。下面是代码示例和结果输出：

```Swift
let lowerCaseString = "hello, world!"
let upperCaseString = lowerCaseString.uppercased()

print(upperCaseString)
// 输出："HELLO, WORLD!"
```

## Deep Dive (深入了解)
在历史上，字符串大小写转换已经存在很长时间，用于各种语言和编程环境。在 Swift 中，`uppercased()` 方法遵循 Unicode 标准对字符串进行大写转换，确保国际化字符也能正确处理。

除了 `uppercased()`，Swift 还有其他方法如 `lowercased()` 用于小写转换，或者 `capitalized` 用于只将每个单词的首字母大写。

大写转换的具体实现涉及到字符编码的映射规则，Unicode 是这个过程的标准。而 Swift 的 `uppercased()` 方法已经处理好了这些细节。

## See Also (参考链接)
- Swift String Documentation: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicode Standard: [Unicode Standard](http://www.unicode.org/standard/standard.html)
