---
title:    "Kotlin: 生成随机数"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

为什么：在编写程序时，生成随机数是必不可少的。它可以用于各种需求，例如模拟游戏中的掷骰子、生成随机密码以及测试程序的随机性等。

如何：在Kotlin中，我们可以使用Random类来生成随机数。首先，我们需要在程序的顶部导入Random类：```Kotlin
import java.util.Random
```

接下来，我们可以创建一个Random对象并使用它的nextInt()方法来生成随机整数。例如，我们想要生成一个1到10之间的随机整数，可以这样写：```Kotlin
val random = Random()
val randomNumber = random.nextInt(10) + 1

println(randomNumber) // 输出可能为1到10之间的任意整数
```

如果我们想要生成一个0到1之间的随机小数，可以使用nextDouble()方法：```Kotlin
val random = Random()
val randomDouble = random.nextDouble() // 输出为0到1之间的任意小数

println(randomDouble)
```

深入了解：在Kotlin中，Random类也可以设置种子值，以控制生成随机数的结果。我们可以通过在创建Random对象时传入一个long型的种子值来实现。例如，我们想要每次生成的随机数都相同，可以这样写：```Kotlin
val random = Random(12345L) // 种子值为12345L
```

此外，Random类还有其他一些方法可以用来生成不同类型的随机数，例如生成高斯分布的随机数、生成随机布尔值等。有兴趣的读者可以深入了解Random类的各种方法。

请参阅：如何使用Kotlin中的Random类来生成随机数：[https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)；使用Kotlin中的Random类来生成随机数的示例代码：[https://github.com/Kotlin/KEEP/blob/master/proposals/higherOrderFunctions.md](https://github.com/Kotlin/KEEP/blob/master/proposals/higherOrderFunctions.md)。