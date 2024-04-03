---
date: 2024-01-26 04:15:58.931367-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u542F\u52A8Kotlin\u7684REPL\u8F7B\u800C\
  \u6613\u4E3E\u3002\u6253\u5F00\u7EC8\u7AEF\u5E76\u8F93\u5165`kotlinc`\u3002\u4F60\
  \u5C06\u8FDB\u5165Kotlin shell\u3002\u6211\u4EEC\u5C1D\u8BD5\u5B9A\u4E49\u4E00\u4E2A\
  \u53D8\u91CF\u5E76\u6253\u5370\u5176\u503C\uFF1A."
lastmod: '2024-03-13T22:44:47.720915-06:00'
model: gpt-4-0125-preview
summary: "\u542F\u52A8Kotlin\u7684REPL\u8F7B\u800C\u6613\u4E3E\u3002\u6253\u5F00\u7EC8\
  \u7AEF\u5E76\u8F93\u5165`kotlinc`\u3002\u4F60\u5C06\u8FDB\u5165Kotlin shell\u3002\
  \u6211\u4EEC\u5C1D\u8BD5\u5B9A\u4E49\u4E00\u4E2A\u53D8\u91CF\u5E76\u6253\u5370\u5176\
  \u503C\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
