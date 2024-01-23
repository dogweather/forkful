---
title:                "生成随机数"
date:                  2024-01-20T17:49:55.914614-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
产生随机数是让程序能自动创建出不可预测值的过程。程序员这么做通常是为了增加娱乐性、测试可能的情景，或实现某些安全特性。

## How to: (如何操作)
Swift 为我们提供了几种生成随机数的方法。以下是一些快速直接的例子：

生成一个随机整数：
```Swift
let randomInt = Int.random(in: 1...10)
print(randomInt)
```
输出样本： `7` （每一次运行结果可能不同）

生成一个随机浮点数：
```Swift
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
```
输出样本： `0.87431` （每一次运行结果可能不同）

生成一个随机布尔值：
```Swift
let randomBool = Bool.random()
print(randomBool)
```
输出样本： `true` 或 `false` （每一次运行结果可能不同）

## Deep Dive (深入探究)
随机数生成有着很长的历史，从简单的物理设备（比如投色子）到复杂的算法。在计算机初期，生成可靠的随机数是一个挑战。早期方法，如线性同余生成器（LCG），易于实现但有已知缺陷。

Swift 使用的是一个更高级的伪随机数生成器（PRNG），这种生成器设计用来通过算法模仿随机性，并在绝大多数应用场景下足够好用。然而，如果你需要更高安全性的随机数（例如加密），应该使用 `Security.framework` 中的 `SecRandomCopyBytes` 函数。

有其他随机数生成方法，例如蒙特卡罗方法，它通常用于更复杂的数学模拟和计算中。

在Swift实现中，各种 `random` 方法通常使用了内置的随机数发生器，它为统一的API背后提供了满足大多数需要的随机性。

## See Also (参考链接)
- [Swift Standard Library Documentation](https://developer.apple.com/documentation/swift/swift_standard_library)
- [RandomNumberGenerator Protocol](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- [Using Cryptographically Secure Random Numbers](https://developer.apple.com/documentation/security/1399291-secrandomcopybytes)
