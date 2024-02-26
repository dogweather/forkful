---
date: 2024-01-26 03:50:10.041151-07:00
description: "\u6DF1\u5165\u8C03\u8BD5\u5668\u7684\u76EE\u7684\u5168\u5728\u4E8E\u9010\
  \u6B65\u6267\u884C\u4EE3\u7801\uFF0C\u89C2\u5BDF\u5176\u8FD0\u4F5C\u8FC7\u7A0B\u5E76\
  \u5F53\u573A\u6355\u6349\u90A3\u4E9B\u8BA8\u538C\u7684\u9519\u8BEF\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\uFF0C\u662F\u56E0\u4E3A\u5B83\u4EEC\u662F\u5E2E\
  \u52A9\u6211\u4EEC\u627E\u51FA\u95EE\u9898\u6240\u5728\u7684\u4FA6\u63A2\u5DE5\u5177\
  \uFF0C\u65E0\u9700\u4E3A\u6B64\u800C\u6293\u72C2\u3002"
lastmod: '2024-02-25T18:49:45.291734-07:00'
model: gpt-4-0125-preview
summary: "\u6DF1\u5165\u8C03\u8BD5\u5668\u7684\u76EE\u7684\u5168\u5728\u4E8E\u9010\
  \u6B65\u6267\u884C\u4EE3\u7801\uFF0C\u89C2\u5BDF\u5176\u8FD0\u4F5C\u8FC7\u7A0B\u5E76\
  \u5F53\u573A\u6355\u6349\u90A3\u4E9B\u8BA8\u538C\u7684\u9519\u8BEF\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\uFF0C\u662F\u56E0\u4E3A\u5B83\u4EEC\u662F\u5E2E\
  \u52A9\u6211\u4EEC\u627E\u51FA\u95EE\u9898\u6240\u5728\u7684\u4FA6\u63A2\u5DE5\u5177\
  \uFF0C\u65E0\u9700\u4E3A\u6B64\u800C\u6293\u72C2\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
