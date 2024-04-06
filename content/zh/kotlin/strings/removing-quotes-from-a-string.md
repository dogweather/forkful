---
date: 2024-01-26 03:40:51.186934-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4ECE\u5386\u53F2\u4E0A\u770B\uFF0C\u5904\
  \u7406\u5B57\u7B26\u4E32\u548C\u8F6C\u4E49\u5B57\u7B26\u4E00\u76F4\u662F\u7F16\u7A0B\
  \u7684\u6838\u5FC3\u90E8\u5206\uFF0C\u56E0\u4E3A\u6587\u672C\u662F\u6211\u4EEC\u4E0E\
  \u6570\u636E\u4EA4\u4E92\u7684\u57FA\u672C\u65B9\u5F0F\u3002\u5B57\u7B26\u4E32\u4E2D\
  \u7684\u5F15\u53F7\u6709\u65F6\u9700\u8981\u88AB\u8F6C\u4E49\u3002\u8FD9\u662F\u901A\
  \u8FC7\u524D\u7F6E\u7684\u53CD\u659C\u6760\u8868\u793A\u7684\uFF08\u4F8B\u5982\uFF0C\
  `\"\u5979\u8BF4\uFF0C\\\"\u55E8\uFF01\\\"\"`\uFF09\u3002\u5904\u7406\u8FD9\u6837\
  \u7684\u5B57\u7B26\u4E32\u65F6\uFF0C\u4F60\u53EF\u80FD\u9700\u8981\u79FB\u9664\u8F6C\
  \u4E49\u5B57\u7B26\u6216\u5F15\u53F7\u672C\u8EAB\uFF0C\u4EE5\u83B7\u5F97\u66F4\u5E72\
  \u51C0\u6216\u66F4\u53EF\u7528\u7684\u6587\u672C\u3002 `replace`\u2026"
lastmod: '2024-04-05T22:51:00.914800-06:00'
model: gpt-4-0125-preview
summary: "`replace` \u65B9\u6CD5\u7684\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\u57FA\u4E8E\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u7684\u79FB\u9664\u6216\u624B\u52A8\u9010\u4E2A\u5B57\
  \u7B26\u89E3\u6790\u5B57\u7B26\u4E32\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u7B80\u5355\
  \u64CD\u4F5C\u6765\u8BF4\uFF0C\u6B63\u5219\u53EF\u80FD\u662F\u8FC7\u5EA6\u7684\uFF0C\
  \u624B\u52A8\u89E3\u6790\u4E5F\u4E0D\u5982\u4F7F\u7528\u5185\u7F6E\u5B57\u7B26\u4E32\
  \u51FD\u6570\u6548\u7387\u9AD8\u3002Kotlin \u7684 `replace` \u51FD\u6570\u5229\u7528\
  \u4E86\u5E95\u5C42 Java \u7684 `String` `replace` \u65B9\u6CD5\uFF0C\u8BE5\u65B9\
  \u6CD5\u5728\u6027\u80FD\u4E0A\u8FDB\u884C\u4E86\u826F\u597D\u7684\u4F18\u5316\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

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
