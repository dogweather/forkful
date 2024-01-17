---
title:                "提取子字符串"
html_title:           "Kotlin: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是提取子串？为什么程序员要这么做？

提取子串指的是从一个字符串中抽取出所需部分。程序员常常需要这么做，是因为字符串可能是非常大的，而我们只需要其中的一部分信息。通过提取子串，我们可以轻松地获取所需信息，而无需遍历整个字符串。

## 如何实现：

下面是几种提取子串的方法示例，结果显示在注释中：

- 使用 `substring()` 方法：从字符串的指定位置开始提取子串，直到结尾或指定结束位置。
```
Kotlin val str = "Hello World"
println(str.substring(6)) // Output: "World"
```

- 使用 `subSequence()` 方法：从字符串的指定范围提取子串，包含第一个索引，但不包含最后一个索引。
```
Kotlin val str = "Hello World"
println(str.subSequence(0, 5)) // Output: "Hello"
```

- 使用 `regex()` 方法：通过正则表达式匹配提取子串。
```
Kotlin val str = "Hello World"
val pattern = "[a-z]".toRegex()
println(pattern.findAll(str).joinToString("")) // Output: "elloorld"
```

## 深入探讨：

历史背景：在过去，程序员使用很多不同的方法来提取子串，包括使用循环和索引操作。Kotlin 提供的 `substring()` 和 `subSequence()` 方法大大简化了这一过程。

其他替代方法：除了 Kotlin 提供的方法，程序员还可以使用 Java 中的 `substring()` 方法来提取子串。

实现细节：Kotlin 的 `substring()` 方法基于 Java 中的 `substring()` 方法，但是它返回的是一个新的字符串而不是 Java 中的 `CharSequence` 类型。

## 链接参考：

- [Kotlin substring() 方法](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Kotlin subSequence() 方法](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/sub-sequence.html)