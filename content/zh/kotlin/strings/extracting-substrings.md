---
aliases:
- /zh/kotlin/extracting-substrings/
date: 2024-01-20 17:46:17.275729-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF0C\
  \u5C31\u662F\u4ECE\u4E00\u4E2A\u8F83\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62FF\u51FA\
  \u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\
  \u4E86\u83B7\u53D6\u6709\u7528\u4FE1\u606F\uFF0C\u6216\u8005\u5BF9\u6570\u636E\u8FDB\
  \u884C\u7279\u5B9A\u7684\u64CD\u4F5C\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.085704
model: gpt-4-1106-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF0C\
  \u5C31\u662F\u4ECE\u4E00\u4E2A\u8F83\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62FF\u51FA\
  \u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\
  \u4E86\u83B7\u53D6\u6709\u7528\u4FE1\u606F\uFF0C\u6216\u8005\u5BF9\u6570\u636E\u8FDB\
  \u884C\u7279\u5B9A\u7684\u64CD\u4F5C\u5904\u7406\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要用？)
从字符串中提取子字符串，就是从一个较长的字符串中拿出一部分。程序员这么做主要是为了获取有用信息，或者对数据进行特定的操作处理。

## How to: (如何操作：)
```kotlin
fun main() {
    val exampleString = "Hello, Kotlin Learners!"

    // 获取特定位置的子字符串
    val substring1 = exampleString.substring(7, 13)
    println(substring1) // 输出: Kotlin

    // 从开始到特定位置的子字符串
    val substring2 = exampleString.substring(0..4)
    println(substring2) // 输出: Hello

    // 使用步长
    val substring3 = exampleString.substring(0..exampleString.length-1 step 2)
    println(substring3) // 输出: Hlo olnLanr!
}
```

## Deep Dive (深入学习)
提取子字符串是字符串操作的基础，早在早期编程语言就已经有这功能。在Kotlin中，我们通常用 `substring` 方法来提取我们想要的部分，而且Kotlin提供了一系列重载方法，让我们可以指定开始和结束的索引，或者直接使用范围来提取。

如果你只需要字符串的一部分，使用 `substring` 可以提高代码的清晰度和效率。其他语言也有类似的功能，比如Python的切片(slice)语法，Java的 `substring` 方法。

Kotlin中的 `substring` 方法是安全的，如果起始索引或结束索引超出字符串的实际长度会抛出 `StringIndexOutOfBoundsException` 异常，所以处理时应该小心。

## See Also (参考链接)
- [Kotlin官方文档：字符串操作](https://kotlinlang.org/docs/sequences.html#string-representation)
- [Stack Overflow上，关于Kotlin中子字符串提取的讨论](https://stackoverflow.com/questions/tagged/kotlin+substring)
- [《Kotlin in Action》书籍](http://manning.com/books/kotlin-in-action)
