---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
随机数生成是编程中常见的一种方法，能生成一系列看似无序的数字。我们使用它是为了增加程序的多样性或模拟可能性结果。

## 如何操作：
在Kotlin中，可以使用`Random`库轻松生成随机数。这是一个示例：

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(0, 100)
    println(randomNumber)
}
```

上述代码将生成0到99之间的随机整数。

## 深入探究
1. **历史背景**: 随机数的概念可以追溯到概率论的产生。在电脑出现之前，人们通过投掷骰子等方法获取随机数。
2. **替代方案**: 除了Kotlin提供的`Random`库，我们还可以使用Java的`Random`类生成随机数，或者使用更加复杂的算法如梅森旋转算法。
3. **实现细节**: Kotlin的`Random`库使用线性同余生成器原理产生伪随机数，其实就是一个数学公式。

## 查看更多
如果你对随机数的生成感兴趣，你可以试试这些链接：
1. Understanding randomness and random number generators in Kotlin: https://proandroiddev.com/understanding-randomness-and-random-number-generators-in-kotlin-634440f1593b
2. Exploring kotlin.random: https://blog.kotlin-academy.com/exploring-kotlin-random-a41316e166cd
3. Kotlin official documentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/