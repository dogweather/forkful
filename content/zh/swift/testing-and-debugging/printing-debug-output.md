---
date: 2024-01-20 17:53:19.907893-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) Swift \u63D0\u4F9B\u4E86 `print`\
  \ \u548C `debugPrint` \u4E24\u4E2A\u51FD\u6570\u6765\u8F93\u51FA\u4FE1\u606F\u5230\
  \u63A7\u5236\u53F0\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.160463-06:00'
model: gpt-4-1106-preview
summary: "Swift \u63D0\u4F9B\u4E86 `print` \u548C `debugPrint` \u4E24\u4E2A\u51FD\u6570\
  \u6765\u8F93\u51FA\u4FE1\u606F\u5230\u63A7\u5236\u53F0."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## How to: (怎么做：)
Swift 提供了 `print` 和 `debugPrint` 两个函数来输出信息到控制台。 

```Swift
let message = "你好，世界！"
print(message) // 普通输出

var numbers = [1, 2, 3, 4, 5]
debugPrint(numbers) // 调试输出，提供更多信息
```

输出：

```
你好，世界！
[1, 2, 3, 4, 5]
```

## Deep Dive (深入探究)
早期编程时代，打印输出通常是唯一调试手段。Swift 的 `print` 非常简便但信息量有限。 `debugPrint` 则给出了结构化信息，更适合复杂数据。你也可以使用 `CustomStringConvertible` 和 `CustomDebugStringConvertible` 协议自定义输出格式。日志框架（如 `OSLog`）是更先进的替代方案，支持日志级别和持久存储。

## See Also (另见)
- Swift 官方文档：[`print(_:separator:terminator:)`](https://developer.apple.com/documentation/swift/1541053-print)
- Apple Developer Documentation：[`OSLog`](https://developer.apple.com/documentation/os/logging)
