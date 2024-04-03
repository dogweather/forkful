---
date: 2024-01-26 03:42:39.657674-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u8BA9\u4F60\u80FD\u591F\u76F8\u5F53\
  \u4FBF\u5229\u5730\u89E3\u51B3\u79FB\u9664\u5F15\u53F7\u7684\u4EFB\u52A1\u3002\u8FD9\
  \u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 `replacingOccurrences(of:with:)` \u7684\u5FEB\
  \u901F\u793A\u4F8B\uFF0C\u5B83\u6B63\u5982\u542C\u8D77\u6765\u7684\u90A3\u6837\u2014\
  \u2014\u7528\u522B\u7684\u4E1C\u897F\u6216\u6839\u672C\u4E0D\u7528\u4E1C\u897F\u6765\
  \u66FF\u6362\u6587\u672C\u7684\u4E00\u90E8\u5206\u3002"
lastmod: '2024-03-13T22:44:48.144281-06:00'
model: gpt-4-0125-preview
summary: "Swift \u8BA9\u4F60\u80FD\u591F\u76F8\u5F53\u4FBF\u5229\u5730\u89E3\u51B3\
  \u79FB\u9664\u5F15\u53F7\u7684\u4EFB\u52A1\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\
  \u7528 `replacingOccurrences(of:with:)` \u7684\u5FEB\u901F\u793A\u4F8B\uFF0C\u5B83\
  \u6B63\u5982\u542C\u8D77\u6765\u7684\u90A3\u6837\u2014\u2014\u7528\u522B\u7684\u4E1C\
  \u897F\u6216\u6839\u672C\u4E0D\u7528\u4E1C\u897F\u6765\u66FF\u6362\u6587\u672C\u7684\
  \u4E00\u90E8\u5206."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

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
