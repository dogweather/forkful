---
title:                "Kotlin: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么

在编程过程中，有时候我们需要从一个字符串中提取出部分内容进行处理。比如，我们可能需要从一个网址中获取域名，或者从一个文本消息中提取出特定关键词。在这种情况下，提取子串是非常有用的。让我们来看看如何在Kotlin中提取子串吧！

## 如何做

提取子串的方法非常简单。我们可以使用 `substring()` 函数来实现。这个函数有两个参数，第一个参数是起始索引，第二个参数是结束索引（可选）。以下是一个示例，展示如何从一个字符串中提取出前五个字符：

```Kotlin
val str = "Hello World!"
val result = str.substring(0, 5)
println(result) // 输出 "Hello"
```

我们还可以使用负数作为索引，来从后往前提取子串。例如，如果我们想从一个字符串中提取出最后五个字符，可以这样做：

```Kotlin
val str = "Hello World!"
val result = str.substring(str.length - 5)
println(result) // 输出 "World"
```

值得注意的是，`substring()` 函数并不会修改原始字符串，而是返回一个新的字符串作为结果。因此，如果我们想要修改原始字符串的话，可以使用 `replaceRange()` 函数来替换指定索引范围内的字符。

## 深入了解

除了 `substring()` 函数，Kotlin还提供了其他几个方法来提取子串：

- `subSequence()`：和 `substring()` 函数类似，但是返回一个 `CharSequence` 类型的结果，可以自动转换为字符串。
- `slice()`：从一个字符串集合中提取出指定索引的子串，并返回一个新的字符串集合作为结果。
- `take()` 和 `drop()`：从一个字符串中提取出指定数量的字符，分别返回前N个字符或后N个字符作为结果。
- `split()`：将一个字符串根据指定的分隔符分割成多个子串，并返回一个包含分割结果的字符串数组。

还有一点需要注意的是，Kotlin中的字符串索引是从0开始的，这和一些其他编程语言有所不同。

## 参考资料

- [Kotlin Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlin String Operations](https://www.baeldung.com/kotlin/string-operations)
- [Kotlin Examples – substring()](https://beginnersbook.com/2019/02/kotlin-string-substring()/)
- [Kotlin String slicing and manipulating using built-in functions](https://medium.com/@anilshanbhag3/kotlin-basic-concepts-string-methods-and-data-types-df854bd85fc8)

# 参考链接

- [Kotlin官方文档](https://kotlinlang.org/docs/)
- [Kotlin for Android Developers](https://antonioleiva.com/kotlin-android-developers-book/)

# 请看

本文介绍了在Kotlin中提取子串的方法，包括如何使用 `substring()` 函数以及其他相关的方法。希望这篇文章能帮助你更好地理解Kotlin字符串的处理方法。如果你想继续学习Kotlin，可以参考上面的参考资料或者浏览其他相关的链接。谢谢阅读！


# 参见

- [Kotlin基础 - Kotlin Playground](https://play.kotlinlang.org/byExample/overview)
- [Kotlin教程 - Runoob](https://www.runoob.com/kotlin/kotlin-basic-syntax.html)
- [Kotlin入门指南 - Tutorialspoint](https://www.tutorialspoint.com/kotlin/index.htm)