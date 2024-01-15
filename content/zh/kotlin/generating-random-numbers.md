---
title:                "生成随机数"
html_title:           "Kotlin: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数字是编程中常用的一项技术，可以用来测试程序、产生不同的数据集等等。使用Kotlin编程语言可以轻松地实现这项技术，下面的文章将介绍如何使用Kotlin生成随机数字及相关的一些深入信息。

## 如何操作

首先，在代码中导入`kotlin.random`包，然后使用`Random()`函数来创建一个随机数生成器对象。

```Kotlin 
import kotlin.random.Random
val random = Random()
```
这样就可以使用`random`对象来生成随机数字了。

#### 生成整数

使用`nextInt()`函数来生成一个随机的整数，可以指定一个范围作为参数，也可以不指定，如下所示：

```Kotlin
val num1 = random.nextInt() // 生成任意范围的整数
val num2 = random.nextInt(10) // 生成0到10的整数
val num3 = random.nextInt(5, 20) // 生成5到20的整数
println("随机生成的整数为： $num1, $num2, $num3")
```
这样就可以得到类似下面的结果：

```
随机生成的整数为： 793966528, 4, 17
```

#### 生成小数

使用`nextDouble()`函数来生成一个随机的小数，也可以指定范围，如下所示：

```Kotlin
val decimal1 = random.nextDouble()  // 生成0到1的小数
val decimal2 = random.nextDouble(50.0) // 生成0到50的小数
val decimal3 = random.nextDouble(10.0, 20.0) // 生成10到20的小数
println("随机生成的小数为： $decimal1, $decimal2, $decimal3")
```

得到的结果可能类似下面的样子：

```
随机生成的小数为： 0.07061632565166759, 23.671395178, 14.1600328567853
```

#### 生成布尔值

使用`nextBoolean()`函数来生成随机的布尔值，结果可能是`true`或者`false`，如下所示：

```Kotlin
val bool1 = random.nextBoolean()
val bool2 = random.nextBoolean()
println("随机生成的布尔值为： $bool1, $bool2")
```

可能得到的结果类似这样：

```
随机生成的布尔值为： true, false
```

## 深入了解

Kotlin中，`Random()`函数内部是使用`java.util.Random`类来生成随机数的。如果想要更复杂的随机数生成，可以使用该类中的其他函数，比如使用`nextInt()`函数来生成一个随机的`Int`数组。

```Kotlin
val random = java.util.Random()
val arr = random.ints(5, 1, 10).toArray() // 生成5个位于1到10之间的随机整数
println("随机生成的数组为： $arr")
```

得到的结果可能类似下面的样子：

```
随机生成的数组为： [6, 3, 1, 2, 7]
```

## 参考链接

- [Kotlin Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Java Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)