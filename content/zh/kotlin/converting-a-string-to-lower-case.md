---
title:                "Kotlin: 将字符串转换为小写"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要把字符串转换为小写字母

在编程中，经常会涉及到对字符串的操作，而字符串的大小写常常会影响到程序的逻辑和结果。因此，将字符串转换为小写字母可以确保程序的正确性和一致性。

## 如何将字符串转换为小写字母

在Kotlin中，有一个内置的方法`toLowerCase()`可以实现将字符串转换为小写字母的功能。下面是一个简单的示例代码，展示了如何使用这个方法：

```Kotlin
val str = "Hello, WORLD!"

println(str.toLowerCase())
```

运行这段代码，输出将会是`hello, world!`。我们可以看到，原本字符串中的大写字母被转换为了小写字母。

## 深入了解字符串转换为小写字母的原理

在Kotlin中，字符串是不可变的，也就是说无法直接修改字符串中的内容。因此，`toLowerCase()`方法实际上是创建了一个新的字符串，其中所有的大写字母被替换为了小写字母。这也符合了函数式编程的思想，保持了原始字符串的不可变性。

除了`toLowerCase()`方法之外，Kotlin还提供了一些其他的方法来操作字符串的大小写，比如`toUpperCase()`、`capitalize()`等。通过学习这些方法，我们可以更加灵活地应对不同的需求。

## 参考资料

- [Kotlin字符串转换为小写字母](https://www.javatpoint.com/kotlin-string-tolowercase)
- [Kotlin官方文档](https://kotlinlang.org/docs/reference/strings.html#string-transformation)

# 参见

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)
- [Kotlin入门教程](https://www.runoob.com/kotlin/kotlin-tutorial.html)