---
date: 2024-01-20 17:56:12.503023-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.736384-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
