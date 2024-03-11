---
date: 2024-01-20 17:53:19.907893-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u5C31\u662F\u5728\u63A7\u5236\u53F0\
  \u663E\u793A\u53D8\u91CF\u548C\u7A0B\u5E8F\u6267\u884C\u4FE1\u606F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u8FD9\u6837\u53EF\u4EE5\u7406\u89E3\u7A0B\
  \u5E8F\u8FD0\u884C\u8FC7\u7A0B\uFF0C\u627E\u51FAbug\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.964522-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u5C31\u662F\u5728\u63A7\u5236\u53F0\
  \u663E\u793A\u53D8\u91CF\u548C\u7A0B\u5E8F\u6267\u884C\u4FE1\u606F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u8FD9\u6837\u53EF\u4EE5\u7406\u89E3\u7A0B\
  \u5E8F\u8FD0\u884C\u8FC7\u7A0B\uFF0C\u627E\u51FAbug\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

打印调试输出就是在控制台显示变量和程序执行信息。程序员这么做是因为这样可以理解程序运行过程，找出bug。

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
