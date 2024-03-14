---
date: 2024-01-20 17:56:12.503023-07:00
description: "\u5728\u547D\u4EE4\u884C\u8BFB\u53D6\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\
  \u6839\u636E\u7528\u6237\u8F93\u5165\u8FD0\u884C\u3002\u4E3A\u4E86\u8BA9\u7A0B\u5E8F\
  \u66F4\u7075\u6D3B\uFF0C\u7A0B\u5E8F\u5458\u9700\u8981\u8FD9\u6837\u505A\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.736384-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u547D\u4EE4\u884C\u8BFB\u53D6\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\
  \u6839\u636E\u7528\u6237\u8F93\u5165\u8FD0\u884C\u3002\u4E3A\u4E86\u8BA9\u7A0B\u5E8F\
  \u66F4\u7075\u6D3B\uFF0C\u7A0B\u5E8F\u5458\u9700\u8981\u8FD9\u6837\u505A\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
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
