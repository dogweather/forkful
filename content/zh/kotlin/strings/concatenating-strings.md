---
date: 2024-01-20 17:35:41.313978-07:00
description: "\u5408\u5E76\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u56E0\u4E3A\u8981\u521B\u5EFA\u4E00\u4E2A\u5305\u542B\u6240\u6709\u4FE1\u606F\u7684\
  \u5355\u4E00\u5B57\u7B26\u4E32\uFF0C\u5F80\u5F80\u7528\u4E8E\u663E\u793A\u6D88\u606F\
  \u3001\u6784\u5EFA\u8F93\u51FA\u683C\u5F0F\u6216\u662F\u751F\u6210\u4EE3\u7801\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.710672-06:00'
model: gpt-4-1106-preview
summary: "\u5408\u5E76\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u56E0\u4E3A\u8981\u521B\u5EFA\u4E00\u4E2A\u5305\u542B\u6240\u6709\u4FE1\u606F\u7684\
  \u5355\u4E00\u5B57\u7B26\u4E32\uFF0C\u5F80\u5F80\u7528\u4E8E\u663E\u793A\u6D88\u606F\
  \u3001\u6784\u5EFA\u8F93\u51FA\u683C\u5F0F\u6216\u662F\u751F\u6210\u4EE3\u7801\u3002\
  ."
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
