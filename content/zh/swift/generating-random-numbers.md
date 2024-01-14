---
title:                "Swift: 生成随机数"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数

在编程中，生成随机数是非常有用的技巧。这可以帮助我们模拟各种情况，测试我们的代码，还可以用于游戏开发中。随机数的生成也可以帮助我们保证代码的安全性，防止他人猜测密码或者攻击我们的系统。总的来说，生成随机数是一项基本的编程技巧，几乎在每个项目中都会用到。

## 如何生成随机数

在Swift中，生成随机数非常简单。我们可以使用`arc4random_uniform(_:)`函数来生成一个指定范围内的随机数。例如，如果我们想要生成一个1到100之间的随机数，我们可以这样写：

```Swift
let randomNum = arc4random_uniform(100) + 1
print(randomNum)
```

这段代码会输出一个在1到100之间的随机数。我们也可以将这个函数封装在一个自定义函数中，以便在多个地方使用。例如：

```Swift
func generateRandomNumber(min: Int, max: Int) -> Int {
    return Int(arc4random_uniform(UInt32(max - min + 1))) + min
}

let randomNum = generateRandomNumber(min: 10, max: 50)
print(randomNum)
```

这段代码会生成一个在10到50之间的随机数。如果我们想要生成一个随机的布尔值，也可以使用`arc4random_uniform(_:)`函数并进行一些简单的操作来实现。例如：

```Swift
let randomBool = arc4random_uniform(2) == 0 ? false : true
print(randomBool)
```

这段代码会输出一个随机的布尔值，即真或假。

## 深入探讨随机数的生成

在Swift中，使用`arc4random_uniform(_:)`函数会返回一个`UInt32`类型的随机数。因此，我们需要根据需求将这个数转换成我们需要的数据类型。例如，如果我们需要一个随机的`Double`或`Float`类型的数，我们可以使用`Double(randomNum)`或`Float(randomNum)`来将`UInt32`类型的数转换成我们需要的数据类型。

除了使用`arc4random_uniform(_:)`函数，Swift还提供了`arc4random()`函数来生成`UInt32`类型的随机数。但是，这个函数返回的是一个比较大范围的随机数，因此我们需要对结果进行压缩，才能得到我们需要的数。例如，如果我们想要生成一个1到10之间的随机数，我们可以这样写：

```Swift
let randomNum = Int(arc4random() % 10) + 1
print(randomNum)
```

这个技巧可以让我们生成更复杂的随机数。同时，Swift也提供了`random()`和`random(in:)`函数来帮助我们生成不同类型的随机数，例如`Int`、`Double`、`Float`、`Bool`等。

## 参考链接

- [Swift官方文档](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Swift 4.2新特性：生成随机数](https://www.hangge.com/blog/cache/detail_2297.html)
- [Swift: 派发随机数](https://www.cnblogs.com/LZY-FE/p/7322698.html)

## 参见

链接：[生成随机数的官方文档](https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID128)