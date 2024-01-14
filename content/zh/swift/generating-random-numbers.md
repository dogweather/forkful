---
title:    "Swift: 生成随机数"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么

生成随机数在编程中是一个很常见的任务。它可以用来创建游戏，测试代码的逻辑，或者简单地为某个数据集添加随机性。无论是什么原因，我们都可以通过使用Swift来轻松地生成随机数。

## 如何

我们可以使用Swift的内置函数 `arc4random()` 来生成一个伪随机的整数。这个函数将会返回一个随机的无符号整数并且保证每次运行的结果都是不同的。

```
Swift
let randomNumber = arc4random()
print(randomNumber)
```

如果我们想要生成一个特定范围内的随机数，我们可以使用 `arc4random_uniform()` 函数，它接受一个整数参数来设置范围。比如，我们想要生成1到100之间的随机数，我们可以这样做：

```
Swift
let randomNumber = arc4random_uniform(100) + 1
print(randomNumber)
```

这将会生成一个1到100之间（包含100）的随机数。

## 深入研究

生成随机数的背后原理涉及到计算机科学中的伪随机数生成器算法。这些算法通过生成伪随机数序列来模拟真正的随机性。如果你对此感兴趣，你可以了解更多关于伪随机数生成和Swift中的随机数函数的实现。

## 参考链接

- [Swift官方文档-随机数函数](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID333)
- [伪随机数生成器算法详解](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [Swift中随机数的实现](https://github.com/apple/swift/blob/main/stdlib/public/Darwin/Arc4Random.swift)