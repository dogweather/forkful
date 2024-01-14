---
title:                "Kotlin: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，我们经常需要使用随机数。生成随机数可以为我们的程序增加不确定性，使其更加有趣和多样化。同时，随机数也可以用于模拟真实世界的情景，并帮助我们测试程序的稳定性。因此，学习如何生成随机数是非常重要的。

## 如何

生成随机数在Kotlin中非常简单。首先，我们需要引入一个名为“Random”的类：

```Kotlin
import kotlin.random.Random
```

接下来，我们可以使用`nextInt()`方法来生成一个随机整数：

```Kotlin
val randomNumber = Random.nextInt()
```

我们也可以指定生成随机数的范围，比如从1到100之间的整数：

```Kotlin
val randomNumber = Random.nextInt(1, 101)
```

如果我们需要生成随机的布尔值，可以使用`nextBoolean()`方法：

```Kotlin
val randomBoolean = Random.nextBoolean()
```

更多的随机数生成方法，可以查看Kotlin官方文档中关于Random类的介绍。

## 深入探讨

生成随机数其实是一个伪随机的过程，因为计算机在运行程序时是按照固定的规则进行操作的。这使得我们不能保证每次运行程序都会得到不同的随机数。为了解决这个问题，我们可以给Random类传入一个种子值，它可以改变随机数生成的规律，从而得到不同的结果。

```Kotlin
val randomNumber = Random(seed = 12345).nextInt()
```

另外，我们还可以通过`Random.Default`来使用默认的随机数生成器，它也可以传入种子值来进行定制。

## 参考链接

- [Kotlin官方文档：Random类](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Kotlin官方文档：Random.Default](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/-default.html)
- [腾讯云：Kotlin随机数的生成和使用方法](https://cloud.tencent.com/developer/article/1385313)
- [开发者头条：使用Kotlin生成随机数](https://toutiao.io/posts/oyyk7q/preview)