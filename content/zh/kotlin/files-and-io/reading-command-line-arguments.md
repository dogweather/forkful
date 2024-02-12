---
title:                "读取命令行参数"
aliases:
- zh/kotlin/reading-command-line-arguments.md
date:                  2024-01-20T17:56:12.503023-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在命令行读取参数允许程序根据用户输入运行。为了让程序更灵活，程序员需要这样做。

## How to: (如何操作：)
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hello, ${args[0]}!")
    } else {
        println("Hello, whoever you are.")
    }
}
```
运行程序：
```
$ kotlinc HelloWorld.kt -include-runtime -d HelloWorld.jar
$ java -jar HelloWorld.jar Kotlin
Hello, Kotlin!
```

## Deep Dive (深入探讨)
命令行参数是程序外部与程序交流的老方法，Unix 和 Windows 命令行工具都用它。Kotlin 直接继承了从 Java 平台的此功能。除了直接使用 `args` 数组，Kotlin 还有库例如 `kotlinx.cli` 以更复杂的方式解析命令行参数。读取命令行参数有多种实现方式，但最基本的就是通过主函数的参数来接收它们。

## See Also (另请参阅)
- [Kotlin Documentation on Getting Started](https://kotlinlang.org/docs/getting-started.html)
- [kotlinx.cli GitHub Repository](https://github.com/Kotlin/kotlinx-cli)
