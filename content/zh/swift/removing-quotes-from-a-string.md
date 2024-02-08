---
title:                "从字符串中移除引号"
aliases:
- zh/swift/removing-quotes-from-a-string.md
date:                  2024-01-26T03:42:39.657674-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

从字符串中移除引号意味着剥去包裹内容的任何引号。我们这么做是为了消毒输入、准备数据存储或去除可能干扰数据处理的不必要文本格式。

## 如何操作：

Swift 让你能够相当便利地解决移除引号的任务。这里有一个使用 `replacingOccurrences(of:with:)` 的快速示例，它正如听起来的那样——用别的东西或根本不用东西来替换文本的一部分。

```swift
var quotedString = "\"这是一个‘被引用的’字符串。\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // 这是一个‘被引用的’字符串。

// 处理单引号？只需更改搜索项。
quotedString = "'这是另一个示例。'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // 这是另一个示例。
```

输出将是完全没有引号的字符串，为你接下来的计划做好准备。

## 深入探讨

我们自编程之初就开始“清理”这样的字符串。早期，更多的是为了节省宝贵的内存和避免在处理输入时出现语法错误。快速前进到今天，这关乎良好的数据卫生——尤其是在处理 JSON 或准备字符串进行数据库工作时。一个偶然的引号可比你说“语法错误”更快地扰乱 SQL 查询。

有替代方法吗？嗯，如果你发现 `replacingOccurrences(of:with:)` 有点太平淡，你可能会深入研究正则表达式来处理更复杂的模式，或者当你只想在特定位置删除引号时。这里 Swift 的 `NSRegularExpression` 类是你的朋友。但记住，正则表达式是一把双刃剑——强大但有时过犹不及。

在实现上，`replacingOccurrences(of:with:)` 是 Swift 中 `String` 提供的一个方法，它内部调用更复杂的字符串操纵函数，处理 Unicode 和现代文本处理的其他复杂性。这是 Swift 处理的那种“表面简单，底层复杂”的情况，所以你不必担心。

## 另请参阅

有关 Swift 中字符串操作的更多信息：

- Swift 编程语言（字符串和字符）：[Swift.org 文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression：[苹果开发者文档](https://developer.apple.com/documentation/foundation/nsregularexpression)

如果你现在对正则表达式感到好奇，并想测试你的模式：

- Regex101：[正则表达式测试器和调试器](https://regex101.com)
