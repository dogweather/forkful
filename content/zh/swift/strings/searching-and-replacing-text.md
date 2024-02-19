---
aliases:
- /zh/swift/searching-and-replacing-text/
date: 2024-01-20 17:58:50.671069-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\uFF0C\u987E\u540D\u601D\u4E49\
  \uFF0C\u5C31\u662F\u5728\u5B57\u7B26\u4E32\u4E2D\u5BFB\u627E\u7279\u5B9A\u7684\u5185\
  \u5BB9\u5E76\u7528\u5176\u4ED6\u5185\u5BB9\u66FF\u4EE3\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u5730\u4FEE\u6539\u4EE3\
  \u7801\u3001\u914D\u7F6E\u6587\u4EF6\u6216\u4EFB\u4F55\u6587\u672C\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.429339
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\uFF0C\u987E\u540D\u601D\u4E49\
  \uFF0C\u5C31\u662F\u5728\u5B57\u7B26\u4E32\u4E2D\u5BFB\u627E\u7279\u5B9A\u7684\u5185\
  \u5BB9\u5E76\u7528\u5176\u4ED6\u5185\u5BB9\u66FF\u4EE3\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u5730\u4FEE\u6539\u4EE3\
  \u7801\u3001\u914D\u7F6E\u6587\u4EF6\u6216\u4EFB\u4F55\u6587\u672C\u6570\u636E\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## What & Why?
搜索和替换文本，顾名思义，就是在字符串中寻找特定的内容并用其他内容替代。程序员这么做主要是为了自动化地修改代码、配置文件或任何文本数据。

## How to:
Swift 提供了强大的字符串处理功能。这里是一个基本的搜索替换示例：

```Swift
var text = "Hello, World!"
text = text.replacingOccurrences(of: "World", with: "Mandarin Reader")
print(text)
```

输出结果：
```
Hello, Mandarin Reader!
```

如果你需要更复杂的搜索替换，比如使用正则表达式：

```Swift
var regexText = "The quick brown fox jumps over 1 lazy dog."
let pattern = "\\d+"
let regex = try! NSRegularExpression(pattern: pattern, options: [])
let range = NSRange(location: 0, length: regexText.utf16.count)
regexText = regex.stringByReplacingMatches(in: regexText, options: [], range: range, withTemplate: "2")
print(regexText)
```

输出结果：
```
The quick brown fox jumps over 2 lazy dogs.
```

## Deep Dive
搜索和替换文本的功能自编程语言诞生之初就广泛地被实现和应用。在 Swift 的世界里，`String` 类型有许多内建的方法，如 `replacingOccurrences(of:with:)` 直接用于字符串替换。使用 `NSRegularExpression` 可以执行更复杂的替换，例如支持正则表达式的模式匹配。你也可以替换字符串的一部分，处理字节序列，甚至在多种编码之间进行转换。值得注意的是，Swift 的字符串处理是 Unicode 兼容的，这保证了其在处理多语言文本时的准确性和灵活性。

## See Also
- Swift 官方文档关于字符串处理的部分：[Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression 类文档：[NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- 正则表达式基础：[Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
