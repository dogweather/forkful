---
aliases:
- /zh/kotlin/removing-quotes-from-a-string/
date: 2024-01-26 03:40:51.186934-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u4ECE\u4F60\u6B63\u5728\u5904\u7406\u7684\u6587\u672C\u6570\u636E\u4E2D\u5254\u9664\
  \u4EFB\u4F55\u5F15\u53F7\u5B57\u7B26\u7684\u5B9E\u4F8B\uFF0C\u65E0\u8BBA\u662F\u5355\
  \u5F15\u53F7\uFF08' '\uFF09\u8FD8\u662F\u53CC\u5F15\u53F7\uFF08\" \"\uFF09\u3002\
  \u7A0B\u5E8F\u5458\u5E38\u5E38\u9700\u8981\u8FD9\u4E48\u505A\u4EE5\u8FDB\u884C\u6570\
  \u636E\u6E05\u6D17\uFF0C\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\u505A\u51C6\u5907\uFF0C\
  \u6216\u8005\u5F53\u5F15\u53F7\u672C\u8EAB\u4E0E\u6570\u636E\u7684\u542B\u4E49\u65E0\
  \u5173\u65F6\u3002"
lastmod: 2024-02-18 23:08:59.084758
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u4ECE\u4F60\u6B63\u5728\u5904\u7406\u7684\u6587\u672C\u6570\u636E\u4E2D\u5254\u9664\
  \u4EFB\u4F55\u5F15\u53F7\u5B57\u7B26\u7684\u5B9E\u4F8B\uFF0C\u65E0\u8BBA\u662F\u5355\
  \u5F15\u53F7\uFF08' '\uFF09\u8FD8\u662F\u53CC\u5F15\u53F7\uFF08\" \"\uFF09\u3002\
  \u7A0B\u5E8F\u5458\u5E38\u5E38\u9700\u8981\u8FD9\u4E48\u505A\u4EE5\u8FDB\u884C\u6570\
  \u636E\u6E05\u6D17\uFF0C\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\u505A\u51C6\u5907\uFF0C\
  \u6216\u8005\u5F53\u5F15\u53F7\u672C\u8EAB\u4E0E\u6570\u636E\u7684\u542B\u4E49\u65E0\
  \u5173\u65F6\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么和为什么？

从字符串中移除引号意味着从你正在处理的文本数据中剔除任何引号字符的实例，无论是单引号（' '）还是双引号（" "）。程序员常常需要这么做以进行数据清洗，为进一步处理做准备，或者当引号本身与数据的含义无关时。

## 如何操作：

以下是在Kotlin中从字符串中移除两种类型引号的简单方式：

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // 输出：Kotlin rocks its cool
}
```

如果你只想移除一种类型的引号，只需跳过另一个替换调用即可。

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // 输出：Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // 输出：Kotlin "rocks" its cool
}
```

## 深入了解

从历史上看，处理字符串和转义字符一直是编程的核心部分，因为文本是我们与数据交互的基本方式。字符串中的引号有时需要被转义。这是通过前置的反斜杠表示的（例如，`"她说，\"嗨！\""`）。处理这样的字符串时，你可能需要移除转义字符或引号本身，以获得更干净或更可用的文本。

`replace` 方法的替代方案包括基于正则表达式的移除或手动逐个字符解析字符串。然而，对于简单操作来说，正则可能是过度的，手动解析也不如使用内置字符串函数效率高。Kotlin 的 `replace` 函数利用了底层 Java 的 `String` `replace` 方法，该方法在性能上进行了良好的优化。

在实现上，值得一提的是，Kotlin 与 Java 是互操作的，所以，实际上，你对字符串执行的任何操作都和在 Java 中一样高效。在移除引号时，意识到边缘情况很关键，像嵌套引号，这可能需要采取更复杂的方法，可能使用正则表达式或解析库。

## 另请参阅

想要了解更多关于Kotlin中处理字符串的上下文，你可以查看官方文档：

- [Kotlin 的字符串文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

深入了解Kotlin中的正则表达式和解析：

- [Kotlin Regex 文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
