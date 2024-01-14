---
title:                "Kotlin: 生成随机数"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么？
生成随机数可能是编程中最基本的技能之一。它可以用于模拟真实世界的随机事件，测试和调试代码，以及生成密码和加密密钥。不管你是新手还是有经验的程序员，掌握生成随机数的能力都是非常重要的。

## 如何？
在Kotlin中生成随机数非常简单。首先，导入随机数类：
```Kotlin
import kotlin.random.Random
```
然后，我们可以使用`Random.nextInt()`方法来生成一个指定范围内的整数：
```Kotlin
val randomInt = Random.nextInt(1, 10)
```
上面的代码将生成一个1到10之间的随机整数，并将其存储在`randomInt`变量中。

我们也可以使用`Random.nextDouble()`来生成一个指定范围内的双精度浮点数：
```Kotlin
val randomDouble = Random.nextDouble(0.5, 1.5)
```
上面的代码将生成一个0.5到1.5之间的随机双精度浮点数，并将其存储在`randomDouble`变量中。

如果你想生成一个随机布尔值，可以使用`Random.nextBoolean()`方法：
```Kotlin
val randomBoolean = Random.nextBoolean()
```

## 深入了解
生成随机数的原理其实是通过伪随机数发生器（PRNG）来实现的。这些算法是基于一种叫做“种子”的初始值，这个值将决定随机数的序列。如果种子相同，那么每次运行程序时都会生成相同的随机数序列。

在Kotlin中，我们可以使用`Random(seed)`来指定种子值来生成随机数。如果没有指定种子，则默认使用当前系统时间。使用相同的种子值将会产生相同的随机数序列，因此在进行测试时非常有用。

## 查看也可以
- [Kotlin官方文档-随机数](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Kotlin官方文档-伪随机数发生器](https://kotlinlang.org/docs/reference/stdlib/kotlin.random/-random/until.html)
- [Medium-Kotlin基础教程-生成随机数](https://medium.com/@mohamedamara/kotlin-generated-random-numbers-tutorial-likes-high-kind-and-random-kind-3dd09bb99426)