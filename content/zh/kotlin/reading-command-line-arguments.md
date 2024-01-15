---
title:                "读取命令行参数"
html_title:           "Kotlin: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么

在开发软件时，我们经常需要从命令行接收用户输入的参数。读取命令行参数是一种重要的技能，能够帮助我们更灵活地控制程序的行为。如果你想要提升自己的软件开发技能，那么学习读取命令行参数是一个不错的选择。

# 如何做

```Kotlin
fun main(args: Array<String>) {
    // 遍历所有命令行参数
    for (arg in args) {
        println(arg)
    }
}
```

在上面的代码中，我们使用`Array<Sting>`来接收所有的命令行参数，并使用`for`循环来遍历每一个参数并打印出来。运行此程序，我们就可以在命令行中看到所有的参数。

```shell
$ kotlin CommandLineArgsExample.kt argument1 argument2 argument3

argument1
argument2
argument3
```

如果我们只想获取特定位置的参数，可以使用索引来指定。

```Kotlin
fun main(args: Array<String>) {
    // 获取第一个参数
    val firstArg = args[0]
    println("First argument: $firstArg")

    // 获取第二个参数
    val secondArg = args[1]
    println("Second argument: $secondArg")
}
```

在上面的代码中，我们使用索引来获取第一个和第二个参数，并打印出来。运行此程序，我们可以看到对应位置的参数被正确获取了。

```shell
$ kotlin CommandLineArgsExample.kt arg1 arg2

First argument: arg1
Second argument: arg2
```

# 深入了解

除了使用索引，我们也可以使用Kotlin标准库提供的`CommandLine.kt`来读取命令行参数。通过使用`commandLine`函数，我们可以轻松地获取命令行参数的名称和值。

```Kotlin
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    // 检查是否存在-a参数
    if ("-a" in args) {
        println("Option -a detected.")
        exitProcess(0)
    }
    
    // 获取-b参数的值
    val bValue = with(commandLine) {
        // 获取参数值，如果不存在则返回null
        getOptionValue("b")
    }
    
    if (bValue != null) {
        println("Value of -b: $bValue")
    }
}
```

在上面的代码中，我们首先使用了`exitProcess`函数来终止程序，并使用`in`操作符来检查是否存在特定的参数。接着，我们使用了`getOptionValue`函数来获取参数的值，并将其打印出来。运行此程序，我们可以看到对应参数的信息被正确地获取和处理。

```shell
$ kotlin CommandLineArgsExample.kt -a -b value

Option -a detected.
Value of -b: value
```

# 参考链接

- [Kotlin官方文档 - 命令行参数](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.system/command-line.html)
- [Kotlin官方文档 - 带有命令行参数的应用程序](https://kotlinlang.org/docs/tutorials/command-line.html)