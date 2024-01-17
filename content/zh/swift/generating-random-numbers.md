---
title:                "生成随机数"
html_title:           "Swift: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Random Number Generation: A Fun Tool for Programmers

## 什么 & 为什么？

随机数生成是指在程序中产生一系列随机数的过程。随机数在编程中是一项非常重要的工具，它可以被用来模拟现实世界的随机性，例如掷骰子、抽奖等。同时，它也可以用来测试程序的稳定性和性能。因此，作为一名程序员，学会生成随机数是很有必要的。

## 如何进行？

生成随机数的方法有很多种，以下为几种常见的方法：

1. 使用系统提供的随机数函数：Swift中提供了`Int.random(in:Range)`函数来生成一个指定范围内的随机整数，`Double.random(in:Range)`函数来生成一个指定范围内的随机浮点数。
```Swift
let randomInt = Int.random(in: 1...10) //生成1到10范围内的随机整数
let randomDouble = Double.random(in: 0..<1.0) //生成0到1范围内的随机浮点数
```

2. 使用第三方库：Swift还有一些第三方库，比如`GameplayKit`和`Random`，它们提供了更多功能丰富的随机数生成方法。
```Swift
import GameplayKit

let randomDistribution = GKRandomDistribution(lowestValue: 0, highestValue: 100) //生成0到100范围内的随机数
let randomNumber = randomDistribution.nextInt() //获取随机数
```

## 深入了解

随机数生成在计算机科学中已经有很悠久的历史。早期的电脑并不能产生真正的随机数，所以人们使用了伪随机数生成算法来模拟随机性。随着技术的发展，现在的电脑可以使用真正的随机数生成器硬件来产生真正的随机数。

除了在编程中常用的伪随机数生成算法外，还有一些其他的种类的随机数生成方法，比如伪随机数序列生成器和真随机数生成器。伪随机数序列生成器可以按照指定的规则生成连续的伪随机数，而真随机数生成器则需要外部的真正随机性源，例如随机数硬件、摄像头的噪声等。

## 参考资料

如果你想更深入地了解随机数生成的知识，可以参考以下资料：

1. [Swift官方文档 - Generating Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID352)
2. [WWDC 2018 - Random numbers in Swift](https://developer.apple.com/videos/play/wwdc2018/223/)
3. [Wikipedia - Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation)