---
title:    "Swift: 生成随机数"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数在计算机编程中扮演着非常重要的角色。它们可以用来增加程序的复杂性并使其更具吸引力。生成随机数的能力也是一个很酷的技巧，它可以让你的程序更活跃有趣。

## 如何做

如果你想在你的应用程序中使用随机数，Swift提供了一种简单的方法来生成它们。在我们开始编写代码之前，先导入Swift的Random数学库。然后，我们可以通过使用`random(in: ...)`方法来生成一个在指定范围内的随机数。例如，如果我们想要生成一个在1到100之间的随机数，我们可以这样写：

```Swift
let randomNumber = Int.random(in: 1...100)
```

这将生成一个随机的整数，它可以用来做各种事情，比如游戏中的赌注或者随机选择一个幸运儿。

## 深入探讨

在Swift中生成随机数的方法有很多种，你可以使用`arc4random_uniform()`方法来生成随机整数，也可以使用`randomElement()`方法来从一个数组中随机选择一个元素。你也可以使用GKRandomSource类来生成更复杂的随机数，例如来自不同分布的随机数。此外，在处理真正的随机性时，需要注意伪随机数生成器的局限性以及如何更好地保护随机数的安全性。

## 参考链接

- [Swift官方文档：Generating Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID524)
- [用Swift生成随机数的最佳方法](https://www.hackingwithswift.com/example-code/language/whats-the-best-way-to-generate-random-numbers-using-swift)
- [如何在游戏中使用随机化](https://gamedevelopment.tutsplus.com/tutorials/random-number-generation-generating-random-sequences--gamedev-1249)
- [随机数生成器的内部原理](https://www.cs160.org/random-generator-security/)