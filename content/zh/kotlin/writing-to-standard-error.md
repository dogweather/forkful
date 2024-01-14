---
title:    "Kotlin: 写入标准错误"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写入标准错误？

标准错误（Standard Error）是与标准输出（Standard Output）一同，用于在命令行界面显示程序的运行结果的一个重要组成部分。在编写Kotlin程序时，需要使用标准错误来输出程序的错误信息，以帮助我们更有效地调试程序。

## 如何写入标准错误

要将错误信息输出到标准错误中，我们可以使用“System.err.println()”方法。这个方法会将括号中的内容打印到标准错误中，并换行。下面是一个简单的示例程序：

```Kotlin
fun main() {
    val num1 = 10
    val num2 = 0
    try {
        val result = num1 / num2
        println("Result: $result")
    } catch (e: Exception) {
        System.err.println("Error: $e")
    }
}
```

上面的程序会尝试计算10除以0的结果，这会导致程序出现错误，并将错误信息打印到标准错误中。运行结果如下：

```
Error: java.lang.ArithmeticException: / by zero
```

## 深入了解

除了使用“System.err.println()”方法，我们还可以使用“System.err.write()”方法将字节数组输出到标准错误中。这样可以更灵活地输出错误信息，以及控制输出的格式。此外，我们还可以使用“System.err.redirect()”方法将标准错误重定向到我们自定义的输出流中，以实现更多的自定义功能。

## 参考链接

- [标准输出与标准错误的区别](https://www.runoob.com/linux/linux-shell.html)
- [Kotlin官方文档](https://kotlinlang.org/docs/reference/compiler-plugins.html)
- [Java标准错误的使用方法](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)