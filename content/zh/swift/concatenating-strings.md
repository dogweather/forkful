---
title:                "字符串拼接"
date:                  2024-01-20T17:36:01.796129-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
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
