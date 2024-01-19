---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何为何缘?
字符串长度就是其包含的字符数，这是计算机程序中常见的操作。程序员需要知道字符串的长度，以便于处理数据外，还有助于调试和优化代码。

## 如何操作:
在 Kotlin 中，你可以使用 `.length` 属性来找到字符串的长度。我们来看一下例子：

```Kotlin
fun main(args: Array<String>) {
  val s = "Hello, Kotlin!"
  println("String length: ${s.length}")
}
```
运行这段代码后，你将看到以下输出：
```Kotlin
String length: 14
```

## 深度解析
在早期的编程语言中，获取字符串长度可能需要通过递归计算字符数。而现在，许多语言（包括 Kotlin）提供内置的属性和方法来找到字符串长度，使得任务十分轻松简单。

取字符串长度的另一种方式是使用 `.count()` 函数。虽然 `.length` 和 `.count()` 都返回字符数，但它们在内部实现上有所不同。`.length` 是一个属性，它会返回字符串中的字符数，而不关心这些字符是什么。`.count()` 是一个函数，它可以接受一个 lambda 表达式作为条件，返回满足条件的字符数。没有参数的 `.count()` 行为与 `.length` 相同。

```Kotlin
fun main(args: Array<String>) {
  val s = "Hello, Kotlin!"
  println("Count: ${s.count()}")
}
```

如果你提供了一个 lambda 表达式作为参数，则 `.count()` 就会返回满足此条件的字符数。例如，下面的代码将返回包含 "o" 的字符数：

```Kotlin
fun main(args: Array<String>) {
  val s = "Hello, Kotlin!"
  println("Number of 'o': ${s.count { it == 'o' }}")
}
```
这段代码将输出 "Number of 'o': 2"。

## 另请参阅
- Kotlin 文档中关于 [`String.length`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html) 和 [`String.count`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/count.html) 的更多信息。
- 阅读 Stack Overflow 上的精彩讨论，了解在不同编程语言中找出字符串长度的不同方法：[How to find the length of a string](https://stackoverflow.com/questions/879531/how-to-find-the-length-of-a-string)
- YouTube 视频教程：[Kotlin Programming - Working with Strings](https://www.youtube.com/watch?v=H_oGi8uuDpA)