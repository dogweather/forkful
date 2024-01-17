---
title:                "开始一个新项目"
html_title:           "Kotlin: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting a New Project in Kotlin: The Basics

## 什么 & 为什么?
启动一个新的项目是指创建一个全新的代码库，用于开发一个新的软件项目。程序员们经常启动新的项目，以实现各种不同的目标，例如增加新的功能，修复错误，或是更新旧的代码。

## 如何:
首先，我们需要安装[Kotlin官方IDE](https://www.jetbrains.com/idea/)，它可以为我们提供代码补全，调试和其他强大的开发工具。然后，我们可以在IDE中创建一个新的Kotlin项目。接下来，我们可以按照以下示例创建一个简单的"Hello World"程序：
```Kotlin
fun main() {
    println("Hello, World!")
}
```
在终端中运行程序的命令是 `kotlinc -script hello.kts`，我们就可以看到输出结果：`Hello, World!`

## 深入探讨:
Kotlin是一种静态类型的编程语言，最初由JetBrains开发，在2011年推出。它可以编译成Java字节码，也可以运行在JVM上，因此可以与Java语言无缝集成。除了可以在服务器端开发大型应用程序之外，Kotlin也可以用于Android应用程序的开发。其他类似的编程语言包括Java，Scala和Groovy。

## 参考资料:
- [Kotlin官方文档](https://kotlinlang.org/docs/home.html)
- [Kotlin Tutorial from w3schools](https://www.w3schools.com/kotlin/)