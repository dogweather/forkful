---
title:                "Swift: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数

当我们编写程序时，有时候需要使用到随机数。随机数能够为程序添加一些不确定性，使得程序表现更加真实和多样化。

## 如何生成随机数

在Swift中，可以使用`arc4random_uniform()`函数来生成随机数。这个函数接受一个整数参数，代表想要生成的随机数的范围。例如，如果我们想生成1到10之间的随机数，可以使用以下代码：

```Swift
let randomNum = arc4random_uniform(10) + 1
```

这将返回一个1到10之间的随机数，并将其存储在变量`randomNum`中。

## 深入了解随机数生成

要想生成更复杂的随机数，可以结合使用其他Swift中的方法。例如，我们可以使用`arc4random()`来生成一个不限制范围的随机数，然后再使用`%`运算符来限制生成的随机数的范围。此外，还可以结合使用时间戳来使得每次生成的随机数都不同。

## 参考资料

- [如何在Swift中生成随机数](https://www.appcoda.com/swift-random-number/)
- [Swift官方文档-生成随机数](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID413)
- [更多关于随机数生成的讨论](https://stackoverflow.com/questions/24007129/how-does-one-generate-a-random-number-in-apples-swift-language)