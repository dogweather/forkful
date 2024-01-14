---
title:                "Kotlin: 读取命令行参数"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

读取命令行参数是编程中一个常见的需求。通过读取命令行参数，您可以在运行程序时向它提供输入，从而使程序更加灵活和可定制。这对于编写可用性强的程序来说至关重要，因此了解如何读取命令行参数是非常重要的。

## 如何做

在Kotlin中，读取命令行参数是非常简单的。首先，您需要使用`args`关键字来声明一个数组来存储命令行参数。然后，您可以使用循环来遍历数组中的参数，并以您需要的方式使用它们。

例如，假设我们的程序需要从命令行接收两个参数：姓名和年龄。那么我们可以这样写代码：

```
fun main(args: Array<String>) { // 使用args关键字声明一个Array来存储命令行参数
    for (arg in args) { // 使用循环遍历数组中的参数
        println("Command line argument: $arg") // 使用println函数打印参数
    }
}
```

假设我们在命令行中输入`kotlin Main.kt John 25`，那么程序的输出将是：

```
Command line argument: John
Command line argument: 25
```

因此，您可以看到如何使用Kotlin来读取和使用命令行参数非常简单。

## 深入了解

实际上，Kotlin中的`main`函数本身就接收一个参数`args`。这个参数就是一个Array，包含了所有的命令行参数。您也可以使用索引来访问特定的参数，例如`args[0]`代表第一个参数。

另外，如果您想要指定参数的类型，您可以使用`args: Array<String>`，其中`String`可以替换为您需要的任何类型，例如`Int`、`Double`等等。这样可以帮助您在使用参数时更加灵活和规范。

另外，您也可以使用Java中的`Scanner`类来读取命令行输入。这种方式可以更加自定义命令行输入的格式，但需要导入Java库。

## 参考链接

- [Kotlin官方文档：读取命令行参数](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-args/index.html)
- [Kotlin For Java Developers](https://www.udemy.com/kotlin-for-java-developers/)
- [Kotlin快速入门教程](https://www.w3cschool.cn/kotlin/)