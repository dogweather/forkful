---
date: 2024-01-20 18:03:48.729941-07:00
description: "How to: \u600E\u6837\u505A\uFF1F Kotlin\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.037898-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u6837\u505A\uFF1F Kotlin \u7531JetBrains\u521B\u5EFA\uFF0C\u9996\u6B21\
  \u4EAE\u76F8\u662F\u57282011\u5E74\u3002\u53D7Java\u8BED\u8A00\u548C\u5176\u4ED6\
  \u73B0\u4EE3\u7F16\u7A0B\u8BED\u8A00\u542F\u53D1\u800C\u6253\u9020\u3002\u4E0EJava\u76F8\
  \u6BD4\uFF0CKotlin\u66F4\u7B80\u6D01\uFF0C\u89E3\u51B3\u4E86\u5F88\u591A\u5E38\u89C1\
  \u7684\u7F16\u7A0B\u95EE\u9898\u3002Kotlin\u53EF\u4EE5\u7F16\u8BD1\u6210Java\u5B57\
  \u8282\u7801\uFF0C\u610F\u5473\u7740\u5B83\u65E2\u53EF\u4EE5\u8FD0\u884C\u5728JVM\u4E0A\
  \uFF0C\u4E5F\u53EF\u4EE5\u8DDFJava\u4EE3\u7801\u65E0\u7F1D\u96C6\u6210\u3002\u4F7F\
  \u7528IntelliJ IDEA\u6216Android Studio\u7684\u5185\u5EFA\u5DE5\u5177\u3001\u547D\
  \u4EE4\u884C\u6216Gradle\u63D2\u4EF6\u521B\u5EFA\u65B0\u9879\u76EE\uFF0C\u90FD\u662F\
  \u5F88\u5E38\u89C1\u7684\u9009\u62E9\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

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
