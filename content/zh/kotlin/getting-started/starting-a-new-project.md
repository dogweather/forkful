---
title:                "开始一个新项目"
aliases: - /zh/kotlin/starting-a-new-project.md
date:                  2024-01-20T18:03:48.729941-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
开始一个新项目就是从零启动你的软件创作过程。程序员这么做是为了解决问题，制作工具，或者表现创意。

## How to: 怎样做？
```Kotlin
// 使用你喜欢的IDE创建一个新的Kotlin项目 或 使用命令行:
// 1. 安装Kotlin编译器：https://kotlinlang.org/docs/command-line.html
// 2. 创建文件 HelloWorld.kt ，然后添加:
fun main() {
    println("你好，世界！")
}

// 3. 命令行运行: kotlinc HelloWorld.kt -include-runtime -d HelloWorld.jar
// 4. 运行Jar文件: java -jar HelloWorld.jar
```

样本输出：
```
你好，世界！
```

## Deep Dive 深入探究
Kotlin 由JetBrains创建，首次亮相是在2011年。受Java语言和其他现代编程语言启发而打造。与Java相比，Kotlin更简洁，解决了很多常见的编程问题。Kotlin可以编译成Java字节码，意味着它既可以运行在JVM上，也可以跟Java代码无缝集成。使用IntelliJ IDEA或Android Studio的内建工具、命令行或Gradle插件创建新项目，都是很常见的选择。

## See Also 另请参阅
- Kotlin官方文档: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- IntelliJ IDEA: [https://www.jetbrains.com/idea/](https://www.jetbrains.com/idea/)
- Android Studio: [https://developer.android.com/studio](https://developer.android.com/studio)
