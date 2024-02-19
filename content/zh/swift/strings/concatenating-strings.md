---
aliases:
- /zh/swift/concatenating-strings/
date: 2024-01-20 17:36:01.796129-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u5730\u521B\
  \u5EFA\u6570\u636E\u5C55\u793A\uFF0C\u6216\u8005\u662F\u4E3A\u4E86\u5728\u4EE3\u7801\
  \u4E2D\u6784\u5EFA\u5177\u4F53\u7684\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.435256
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u5730\u521B\
  \u5EFA\u6570\u636E\u5C55\u793A\uFF0C\u6216\u8005\u662F\u4E3A\u4E86\u5728\u4EE3\u7801\
  \u4E2D\u6784\u5EFA\u5177\u4F53\u7684\u4FE1\u606F\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

字符串拼接就是将两个或多个字符串合并成一个新的字符串。程序员这样做是为了动态地创建数据展示，或者是为了在代码中构建具体的信息。

## How to: (如何操作：)

```Swift
let greeting = "你好, "
let name = "小明!"
let welcomeMessage = greeting + name
print(welcomeMessage) // 输出："你好, 小明!"
```

使用 `+` 运算符简单地将字符串连接起来。还可以这样做：

```Swift
var message = "这是"
message += "一个测试。"
print(message) // 输出："这是一个测试。"
```

或者使用字符串插值：

```Swift
let temperature = 22
let weatherMessage = "今天的气温是\(temperature)°C。"
print(weatherMessage) // 输出："今天的气温是22°C。"
```

## Deep Dive (深入探索：)

历史上，不同的编程语言有不同的字符串拼接方式。早期的语言可能需要调用特定的函数，但在 Swift 中，通过 `+` 运算符和字符串插值两种方式简化了字符串的拼接。

除了 `+` 和字符串插值，`NSString` 类中的 `appending()` 方法也可以用来拼接字符串，特别是当涉及到复杂的字符串处理时。

性能方面，当拼接很长的字符串或者在循环中重复拼接时，应考虑使用 `String` 的 `append()` 方法或可变字符串 `NSMutableString`，以避免不必要的性能开销。

## See Also (参考链接：)

- Apple官方文档有关字符串拼接：[String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- `NSString` 类文档：[NSString](https://developer.apple.com/documentation/foundation/nsstring)
- Swift字符串性能优化：[Optimizing Swift build performance](https://developer.apple.com/swift/blog/?id=37)
