---
date: 2024-01-20 17:35:41.313978-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) Kotlin \u4E2D\uFF0C\u6709\u51E0\u79CD\
  \u65B9\u5F0F\u53EF\u4EE5\u5408\u5E76\u5B57\u7B26\u4E32\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.029397-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u505A\uFF1A) Kotlin \u4E2D\uFF0C\u6709\u51E0\u79CD\u65B9\u5F0F\
  \u53EF\u4EE5\u5408\u5E76\u5B57\u7B26\u4E32\uFF1A."
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to: (如何做：)
Kotlin 中，有几种方式可以合并字符串：

```kotlin
fun main() {
    val greeting = "你好"
    val subject = "世界"

    // 使用加号 (+)
    val message1 = greeting + "，" + subject + "！"
    println(message1) // 输出：你好，世界！

    // 使用字符串模板
    val message2 = "$greeting，$subject！"
    println(message2) // 输出：你好，世界！

    // 使用 joinToString 函数
    val words = listOf(greeting, subject+"！")
    val message3 = words.joinToString("，")
    println(message3) // 输出：你好，世界！
}
```

## Deep Dive (深入探究)
早期的编程语言中，字符串合并可能不那么直接。比如，在C语言中，你得用函数比如 `strcat()` 来实现。在 Java 中，字符串是不可变的，合并时实际上是创建了一个新的字符串对象。

在 Kotlin，我们更常用字符串模板，这是 Kotlin 于 2016 年首次引入率先使用的特性之一。模板会在运行时计算并插入变量的值，这样可读性和效率都很好。若担心性能（如大量拼接操作），你可以考虑使用 `StringBuilder`。

Alternatives, like `StringBuilder`, are available and recommended when dealing with a large number of concatenations, as they can significantly improve performance by reducing the number of temporary objects created.

```kotlin
fun main() {
    // 使用 StringBuilder 来合成字符串
    val builder = StringBuilder()
    builder.append("你好")
           .append("，")
           .append("世界")
           .append("！")
    
    val message = builder.toString()
    println(message) // 输出：你好，世界！
}
```

另外，`+` 运算符虽然简单，但在合并多个字符串时会创建很多临时对象，对性能有负面影响。

## See Also (延伸阅读)
- Kotlin 官方文档：[字符串模板和表达式](https://kotlinlang.org/docs/basic-syntax.html#using-string-templates)
- 性能对比：[StringBuilder vs String concatenation](https://medium.com/@appmattus/effectively-using-kotlin-ranges-15f5ab0473b1)
