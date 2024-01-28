---
title:                "使用调试器"
date:                  2024-01-26T03:50:10.041151-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
深入调试器的目的全在于逐步执行代码，观察其运作过程并当场捕捉那些讨厌的错误。程序员使用调试器，是因为它们是帮助我们找出问题所在的侦探工具，无需为此而抓狂。

## 如何操作:
这里有一小段使用 IntelliJ IDEA（IDE界的夏洛克·福尔摩斯）对 Kotlin 进行调试的示例：

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("猜测数字：")
        guess = readLine()?.toIntOrNull() ?: continue // 忽略错误输入

        // 在此处设置断点，以观察 'guess' 的动作
        if (guess < mysteryNumber) {
            println("太低了！")
        } else if (guess > mysteryNumber) {
            println("太高了！")
        }
    }

    println("你猜对了！神秘数字是 $mysteryNumber")
}
```

调试器输出：
```
猜测数字：
10
太低了！
猜测数字：
50
太高了！
猜测数字：
42
你猜对了！神秘数字是 42
```

## 深入探讨
调试器从50年代开始就出现在游戏中了。那时候，它们相当原始，调试工作更多的是关于硬件而非软件。如今，像 IntelliJ IDEA 中的调试器这样的工具，让我们能够设置断点，一行一行地逐步执行代码，并随意检查变量的状态。

虽然 IntelliJ 的调试器对于 Kotlin 来说非常方便，但它并不是唯一的选择。还有一系列其他选择，如用于 Android 开发的 Logcat，或是面向简洁主义者的命令行工具 jdb。这里的底层魔法大多与 JVM 工具接口（JVMTI）有关，它允许调试器与 Java 虚拟机交互，让 Kotlin 开发者能够参与其中。

## 参见
- IntelliJ IDEA 调试器文档: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
