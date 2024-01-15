---
title:                "生成随机数字"
html_title:           "Swift: 生成随机数字"
simple_title:         "生成随机数字"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数在编程中是一项常见的任务。它可以用于模拟游戏，生成测试数据，或者在任何需要随机性的场景中。Swift 内置了生成随机数的功能，让我们来看看如何使用它！

## 如何

```Swift
// 生成一个随机的布尔值
let randomBool = Bool.random()
// 输出结果：true 或 false

// 生成一个介于 0 到 10 之间的随机数
let randomInt = Int.random(in: 0...10)
// 输出结果：0、1、2、3、4、5、6、7、8、9 或 10

// 生成一个介于 0.0 到 1.0 之间的随机数
let randomDouble = Double.random(in: 0.0..<1.0)
// 输出结果：介于 0.0 到 1.0 之间的小数，不包括 1.0

// 从一个数组中随机选择一个元素
let fruits = ["apple", "orange", "banana", "kiwi"]
let randomFruit = fruits.randomElement()
// 输出结果：数组中的一个水果，比如 "kiwi"
```

## 深入了解

Swift 中的随机数生成功能是由 `RandomNumberGenerator` 协议驱动的。它定义了一个 `next()` 方法，用于生成下一个随机数。Swift 提供了默认的随机数生成器 `RandomNumberGenerator.default`，也可以自定义一个遵循 `RandomNumberGenerator `协议的结构体或类来实现自定义的随机数生成策略。

除了前面提到的方法，Swift 还提供了更多生成随机数的方式，比如 `random(in:using:)`、`shuffled()` 等。详细的文档可参考 [官方文档](https://developer.apple.com/documentation/swift/randomnumbers)。

## 参考资料

- [官方文档](https://developer.apple.com/documentation/swift/randomnumbers)
- [Swift by Sundell - Randomness in Swift](https://www.swiftbysundell.com/articles/randomness-in-swift/)
- [Hacking with Swift - Random numbers in Swift](https://www.hackingwithswift.com/articles/101/random-numbers-in-swift)

## 参见

如需了解更多 Swift 的基础知识和使用技巧，请参考 [Swift 中文指南](https://swiftgg.gitbook.io/swift/).