---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？
新建一个项目，就是用编程语言和工具，从零开始构建一个完全新的软件应用。程序员新建项目，可以由此实现创新的想法，解决问题，甚至创造业界新的标准。

## 如何操作：
我们以使用 Kotlin 以及它的构建工具 Gradle 来创建一个新的项目为例。首先你得安装好 Kotlin 编译器和 Gradle。

```Kotlin
// 1. 创建一个新的目录
mkdir MyNewProject

// 2. 使用 Gradle 进行初始化
cd MyNewProject
gradle init --dsl kotlin --type basic

// 3. 使用 Kotlin 创建一个新的 Hello World 程序
echo 'fun main() { println("Hello world!") }' > src/main/kotlin/Main.kt

// 4. 使用 Gradle 构建并运行该项目
gradle run
```

上述代码执行后，终端中应该会打印出 "Hello world!"。

## 深入探讨：
我们通常从新建一个项目开始，是因为这样可以确保我们有完全的控制权，以及能完全理解项目的每一个部分。新建项目也是理解和学习新的编程语言的好方法。

Kotlin 是由 JetBrains 开发的一种静态类型编程语言，运行在 Java 虚拟机上，并且可以完全兼容 Java 语言。 Kotlin 版的 Hello World 程序包含了一个包含 main 函数的对象。它可以直接被运行，而不需要像 Java 一样打包成一个类库。

最后，你也许会想，为什么我们选择 Gradle 而不是其他构建工具呢？事实上，虽然 Maven 和 Ant 等其他工具也都十分强大，但是 Gradle 的灵活性以及它的 Kotlin DSL，使得我们可以更加容易地配置和构建我们的 Kotlin 项目。

## 参考：
- [Installation guide for Kotlin compiler](https://kotlinlang.org/docs/command-line.html)
- [Gradle tutorial for Kotlin DSL](https://docs.gradle.org/current/userguide/kotlin_dsl.html)
- [Kotlin and Java interoperability](https://kotlinlang.org/docs/java-interop.html)