---
date: 2024-01-26 03:50:10.041151-07:00
description: "\u8FD9\u91CC\u6709\u4E00\u5C0F\u6BB5\u4F7F\u7528 IntelliJ IDEA\uFF08\
  IDE\u754C\u7684\u590F\u6D1B\u514B\xB7\u798F\u5C14\u6469\u65AF\uFF09\u5BF9 Kotlin\
  \ \u8FDB\u884C\u8C03\u8BD5\u7684\u793A\u4F8B\uFF1A ```kotlin fun main() { val mysteryNumber\
  \ = 42 var guess = 0 while (guess != mysteryNumber) {\u2026"
lastmod: '2024-03-13T22:44:47.724251-06:00'
model: gpt-4-0125-preview
summary: "\u8FD9\u91CC\u6709\u4E00\u5C0F\u6BB5\u4F7F\u7528 IntelliJ IDEA\uFF08IDE\u754C\
  \u7684\u590F\u6D1B\u514B\xB7\u798F\u5C14\u6469\u65AF\uFF09\u5BF9 Kotlin \u8FDB\u884C\
  \u8C03\u8BD5\u7684\u793A\u4F8B\uFF1A\n\n```kotlin\nfun main() {\n    val mysteryNumber\
  \ = 42\n    var guess = 0\n\n    while (guess ."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
