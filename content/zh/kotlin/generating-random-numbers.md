---
title:                "生成随机数"
date:                  2024-01-20T17:49:20.969213-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
生成随机数就是创建没有规律、不可预测的数字。程序员需要它们进行测试、数据加密、游戏开发等。

## How to (怎么做)
### Basic Random Numbers (基础随机数)

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt()
    println(randomNumber)
}
```

输出：`-1188957731` （每次运行都会不一样）

### Random Numbers in a Range (指定范围的随机数)

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumberInRange = Random.nextInt(1, 100)
    println(randomNumberInRange)
}
```

输出：`42` （每次运行都会是1到99之间的一个数）

### Random Double (随机双精度浮点数)

```Kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble()
    println(randomDouble)
}
```

输出：`0.7347231747321653` （每次运行都会是0到1之间的一个数）

## Deep Dive (深入探讨)
Kotlin 中的随机数生成背后使用了 Java 的 `java.util.Random` 类。历史上，生成随机数依赖了很多算法，例如线性同余生成器。Kotlin 为这个功能提供了更简洁的接口。

替代方法包括使用 `ThreadLocalRandom` 或 `SecureRandom`，它们在特定情境下更合适，例如多线程或需要高安全标准的密码生成。

在实现时，重要的是考虑生成的随机数是否满足你的需求—是需要伪随机还是加密级别的随机性。

## See Also (另请参阅)
- Kotlin 官方文档关于随机数: [Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Java Platform SE 关于 `Random` 类的文档: [java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
