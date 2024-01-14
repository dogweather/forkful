---
title:                "Kotlin: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么
命令行参数是一种非常常用和重要的编程概念，它允许我们通过命令行来向程序传递输入值。通过阅读本文，您将学习如何在Kotlin中读取命令行参数，并为您的程序增加更多的灵活性。

## 如何读取命令行参数

在Kotlin中，我们可以使用args变量来访问从命令行传递的参数。以下是一个简单的示例程序，它将打印出传递的参数数量和每个参数的值：

```Kotlin
fun main(args: Array<String>) {
  // 打印参数数量
  println("参数数量: ${args.size}")

  // 遍历输出每个参数的值
  for (arg in args) {
    println("参数值: $arg")
  }
}
```

假设我们在命令行运行这个程序，使用“kotlin CommandLineArgs.kt arg1 arg2”作为命令，将会得到如下输出：

```
参数数量: 2
参数值: arg1
参数值: arg2
```

我们也可以在程序中使用命令行参数，比如假设我们的第一个参数表示一个数字，我们可以将它转换成整数并进行计算：

```Kotlin
fun main(args: Array<String>) {
  // 获取第一个参数，并将其转换成整数
  val num1 = args[0].toInt()

  // 获取第二个参数，并将其转换成整数
  val num2 = args[1].toInt()

  // 计算两个数字的和并打印结果
  val sum = num1 + num2
  println("$num1 + $num2 = $sum")
}
```

## 深入学习命令行参数

除了基本的读取命令行参数，您还可以通过使用各种库来增强程序的功能。例如，您可以使用ArgParser库来解析复杂的命令行参数，或者使用Args4j库来构建符合Unix风格的命令行工具。

此外，您还可以了解如何在Android开发中读取命令行参数，以及如何通过Gradle插件来传递命令行参数。

# 参考链接

- [Kotlin命令行参数教程](https://www.journaldev.com/23530/kotlin-command-line-arguments)
- [使用ArgParser解析命令行参数](https://stormpath.com/blog/kotlin-argparser)
- [使用Args4j构建命令行工具](https://medium.com/@danilovf/command-line-interface-with-kotlin-and-args4j-d122e0df920b)
- [在Android中读取命令行参数](https://android.jlelse.eu/what-are-command-line-arguments-and-how-to-read-them-dace41930f4)
- [通过Gradle传递命令行参数](https://kotlinexpertise.com/command-line-parameters-kotlin-gradle/)

# 参见

- [Kotlin官方文档](https://kotlinlang.org/docs/home.html)
- [Kotlin中文社区](https://www.kotlincn.net/)