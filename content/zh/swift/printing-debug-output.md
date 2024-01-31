---
title:                "打印调试输出"
date:                  2024-01-20T17:53:19.907893-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"

category:             "Swift"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/printing-debug-output.md"
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
