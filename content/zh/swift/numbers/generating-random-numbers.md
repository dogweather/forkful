---
title:                "生成随机数"
aliases:
- /zh/swift/generating-random-numbers.md
date:                  2024-01-27T20:35:24.542974-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中生成随机数是关于创建非确定性或不可预测的数值。程序员出于各种原因使用随机数，如在游戏中模拟不可预测性、从数据集中选择随机样本，或用于密码学目的。

## 如何操作：

Swift 通过其标准库提供了一种直接的方式来生成随机数。以下是对不同数值类型进行操作的方法：

```Swift
// 生成一个在0到Int.max之间的随机整数
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// 生成一个在0.0到1.0之间的随机浮点数
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// 生成一个随机Bool值
let randomBool = Bool.random()
print(randomBool)
```

样本输出可能会有变化，因为，毕竟，我们正在处理随机性。多次运行代码会产生不同的数字和布尔值。

## 深入探讨

Swift 的随机数生成方法是建立在一个健壮而有效的伪随机数生成器（PRNG）之上的。在 Swift 4.2 之前，开发人员依赖于外部库或底层平台能力，这可能导致在不同平台和环境中的不一致。通过在 Swift 4.2 中引入原生API，生成随机数变得更简单且更一致，不管底层平台如何。

然而，至关重要的是要理解，Swift 中的标准随机数生成器不适合加密目的。对于密码学，开发人员应该使用苹果平台上的 `Security` 框架，它提供了访问密码学安全随机字节的能力。截至最后更新，Swift 的标准库中不包含跨平台的密码学随机数生成器，这促使开发人员在非苹果平台上寻求第三方库来满足这种需求。

在科学计算领域或需要确定性序列的伪随机数（即可以精确复制的序列）的情况下，如果没有能力给生成器设置种子，Swift 的随机数生成可能不是最佳选择。在这种情况下，通常会使用专门的库和算法来满足这些精确要求。
