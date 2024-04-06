---
date: 2024-01-20 17:56:12.503023-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u547D\u4EE4\u884C\u53C2\u6570\
  \u662F\u7A0B\u5E8F\u5916\u90E8\u4E0E\u7A0B\u5E8F\u4EA4\u6D41\u7684\u8001\u65B9\u6CD5\
  \uFF0CUnix \u548C Windows \u547D\u4EE4\u884C\u5DE5\u5177\u90FD\u7528\u5B83\u3002\
  Kotlin \u76F4\u63A5\u7EE7\u627F\u4E86\u4ECE Java \u5E73\u53F0\u7684\u6B64\u529F\u80FD\
  \u3002\u9664\u4E86\u76F4\u63A5\u4F7F\u7528 `args` \u6570\u7EC4\uFF0CKotlin \u8FD8\
  \u6709\u5E93\u4F8B\u5982 `kotlinx.cli`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.944953-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u547D\u4EE4\u884C\u53C2\u6570\u662F\u7A0B\
  \u5E8F\u5916\u90E8\u4E0E\u7A0B\u5E8F\u4EA4\u6D41\u7684\u8001\u65B9\u6CD5\uFF0CUnix\
  \ \u548C Windows \u547D\u4EE4\u884C\u5DE5\u5177\u90FD\u7528\u5B83\u3002Kotlin \u76F4\
  \u63A5\u7EE7\u627F\u4E86\u4ECE Java \u5E73\u53F0\u7684\u6B64\u529F\u80FD\u3002\u9664\
  \u4E86\u76F4\u63A5\u4F7F\u7528 `args` \u6570\u7EC4\uFF0CKotlin \u8FD8\u6709\u5E93\
  \u4F8B\u5982 `kotlinx.cli` \u4EE5\u66F4\u590D\u6742\u7684\u65B9\u5F0F\u89E3\u6790\
  \u547D\u4EE4\u884C\u53C2\u6570\u3002\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u6709\
  \u591A\u79CD\u5B9E\u73B0\u65B9\u5F0F\uFF0C\u4F46\u6700\u57FA\u672C\u7684\u5C31\u662F\
  \u901A\u8FC7\u4E3B\u51FD\u6570\u7684\u53C2\u6570\u6765\u63A5\u6536\u5B83\u4EEC\u3002"
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
