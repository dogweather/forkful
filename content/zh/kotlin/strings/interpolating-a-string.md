---
date: 2024-01-20 17:51:09.194177-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.703670-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## How to: (如何操作：)
```kotlin
fun main() {
    val name = "张三"
    val age = 28
    println("姓名：$name, 年龄：$age") // 输出: 姓名：张三, 年龄：28

    val message = "下面是你的个人信息:\n姓名：$name\n年龄：${age + 2}"
    println(message) // 输出: 下面是你的个人信息:
                     // 姓名：张三
                     // 年龄：30
}
```

## Deep Dive (深入探索)
字符串插值最早出现在Shell和Perl脚本语言中，后来被Ruby等其他语言采用。它的优点是代码可读性强，可以直接观察到字符串的最终形态。在Kotlin中，通过`$`符号后跟变量名或`${}`包含表达式的方式来插值。在编译成字节码后，Kotlin的字符串插值由`StringBuilder`处理，保证了运行性能。

## See Also (延伸阅读)
- 《Kotlin in Action》 - 了解更深层次的Kotlin开发知识
- 探索字符串插值在其他编程语言中的使用情况
