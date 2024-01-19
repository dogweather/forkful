---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串插值是一种编程模式，它能将变量或表达式直接嵌入到字符串中。程序员之所以这么做，是因为它使得编写和理解字符串更加方便，明了。

## 怎么做？:
下面是一个Kotlin编程语言的例子，这个例子讲解了如何在字符串中插入变量和表达式。

```Kotlin
fun main() {
    val name = "张三"
    val age = 30

    // 使用字符串插值
    println("我是$name, 我的年龄是$age。")

    // 使用表达式
    println("十年后，我${age + 10}岁了。")
}
```

运行这段程序，您将看到下面的输出。

```
我是张三, 我的年龄是30。
十年后，我40岁了。
```

## 深度挖掘
字符串插值是很早就有的概念，在历史中不同的编程语言有不同的实现方式。如今它已经被许多现代编程语言，包括Kotlin，广泛的采用。然而也有一些其他的方式构造字符串，例如：使用字符串连接方法。

在Kotlin 中，字符串插值的实现方式就是通过'$'字符来表示。当Kotlin解析字符串时，遇到'$'字符，就会认为其后的内容需要被插值。

## 另请参阅
如果你还想了解更多关于Kotlin编程语言和字符串插值的信息，那么你可以参考下面的链接。

- [Kotlin语言官方文档](https://kotlinlang.org/)
- [JetBrains公司的Kotlin程序设计语言教程](https://www.jetbrains.com/zh-cn/kotlin/)
- [菜鸟教程上关于Kotlin字符串插值的讲解](https://www.runoob.com/kotlin/kotlin-strings.html)