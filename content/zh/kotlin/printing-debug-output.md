---
title:    "Kotlin: 印刷调试输出。"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 为什么

在编写代码时，打印调试输出是非常重要的一步。它可以帮助我们更好地理解代码运行时的各种情况，帮助我们找出错误和改进代码。因此，学习如何打印调试输出是每个程序员都应该掌握的基本技能。

# 如何做

在Kotlin中，打印调试输出非常简单。我们可以使用标准库中的`println()`函数来打印输出。例如，如果我们想要打印一个变量的值，我们可以使用以下代码：

```Kotlin
val num = 10
println("变量num的值为：$num")
```

当我们运行以上代码时，控制台会输出：`变量num的值为：10`。我们也可以在打印输出中使用字符串模板来输出更复杂的信息。例如：

```Kotlin
val name = "小明"
val age = 20
println("$name的年龄是：$age")
```

上面的代码会输出：`小明的年龄是：20`。

# 深入了解

除了使用`println()`函数，我们也可以使用`print()`函数来打印输出。它们之间的区别在于，`println()`函数会在输出结束后换行，而`print()`函数不会。我们还可以在打印输出中使用格式化字符串来控制输出的格式，例如：

```Kotlin
val num = 3.1415926
println("π的近似值为：%.2f".format(num))
```

上面的代码会输出：`π的近似值为：3.14`。在进行调试时，我们也可以使用`debug()`函数来打印调试信息，它会在日志中显示调用它的文件名和行数。

# 参考链接

- Kotlin官方文档：https://kotlinlang.org/docs/reference/
- 深入理解Kotlin的printf和print函数：https://www.jianshu.com/p/a3b36f1bc899
- Kotlin调试技巧：https://www.programiz.com/kotlin-programming/debugging