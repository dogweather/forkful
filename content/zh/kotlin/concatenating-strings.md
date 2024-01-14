---
title:                "Kotlin: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

拼接字符串是在Kotlin编程中常用的操作，它能够将不同的字符串连接起来形成一个新的字符串。通过拼接字符串，我们可以动态地组合各种不同的文本，从而实现更灵活的数据处理和输出。

## 如何操作

```Kotlin
fun main() {
    // 定义两个字符串
    val str1 = "Hello"
    val str2 = "world"
    
    // 拼接字符串
    val result = str1 + " " + str2
    println(result)
}

// 输出：Hello world
```

在上面的例子中，我们首先定义了两个字符串变量，然后使用加号操作符将它们连接起来，最后通过`println()`函数将结果输出到控制台。除了使用加号操作符，Kotlin还提供了其他方法来拼接字符串，比如使用字符串模板、`StringBuilder`类等，具体可以根据具体需求来选择合适的方法。

## 深入了解

在Kotlin中，字符串是不可变的，也就是说一旦创建后就无法修改其中的内容。因此，当我们需要在一个字符串后面添加另一个字符串时，实际上是创建了一个新的字符串对象，而原来的字符串对象并没有发生改变。此外，拼接字符串的效率也没有直接修改字符串高，因此在进行大量字符串操作时，建议使用`StringBuilder`类，它可以提高性能。

## 参考链接

- Kotlin中的字符串操作：https://www.jianshu.com/p/89af13aaeb73
- Kotlin中的字符串模板：https://www.kotlincn.net/docs/tutorials/kotlin-for-py/string-templates.html
- Kotlin中的StringBuilder类：https://www.kotlincn.net/docs/reference/basic-types.html#string-literals