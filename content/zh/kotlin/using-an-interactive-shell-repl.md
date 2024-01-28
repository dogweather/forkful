---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:15:58.931367-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么及为什么？
REPL（读取-求值-打印循环）是一个简单的交互式计算机编程环境。程序员使用它进行快速编码尝试、测试代码片段，或在不创建完整应用程序的情况下学习一种语言的语法。

## 如何操作：
启动Kotlin的REPL轻而易举。打开终端并输入`kotlinc`。你将进入Kotlin shell。我们尝试定义一个变量并打印其值：

```kotlin
欢迎使用 Kotlin 版本 1.7.10 (JRE 1.8.0_292-b10)
输入 :help 获取帮助，:quit 退出
>>> val greeting = "Hello, Kotlin REPL!"
>>> println(greeting)
Hello, Kotlin REPL!
```

## 深入了解
Kotlin的REPL自该语言推出之时便鼓励实验。它类似于Python的交互式shell，但针对Kotlin的语法和特性进行了调整。其他选择？集成开发环境中的交互式环境，如IntelliJ IDEA，以及在线Kotlin playgrounds。REPL通过即时编译代码工作，提供即时反馈——对学习和调试至关重要。

## 另请参阅
- Kotlin有关REPL的文档：[https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- 在浏览器中尝试Kotlin：[https://play.kotlinlang.org](https://play.kotlinlang.org)
- IntelliJ IDEA中的JetBrains Kotlin Playground插件。
