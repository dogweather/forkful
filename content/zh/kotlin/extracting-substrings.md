---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？

子字符串的提取是从长字符串中获取更小部分的过程。程序员进行子字符串提取通常是因为他们只对字符串的某一部分感兴趣。

## 如何操作：

在Kotlin中，我们可以使用`substring()`函数来提取字符串的一部分。让我们看看如何操作。

```Kotlin
val str = "你好，世界"
val subStr = str.substring(0, 2)

println(subStr)  // 输出：你好
```

在这个例子中，我们提取了位于0和1索引处的字符，Kotlin里的索引是从0开始的。

## 深度挖掘：

在Kotlin中，`substring()`方法的起源可以追溯到早期的编程语言，例如Fortran和C。这种方法在几乎所有高级编程语言中都可以找到。

在提取子字符串时，有许多替代方案可以选择。例如，你可以使用`slice()`函数，它以范围作为参数。使用范围你可以选择你想要的任何索引组合。

实现细节上，`substring()`函数在内部使用了`subSequence()`函数，并且返回类型被转换为String。

```Kotlin
val str = "你好，世界"
val subStr = str.slice(0..1)

println(subStr)  // 输出：你好
```

注意，`slice()`比`substring()`更灵活，因为它允许非连续索引的范围。

## 参见：

有关Kotlin字符串操作的更多信息，可以参考：
- [Kotlin官方文档](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin字符串提取教程](https://www.programiz.com/kotlin-programming/string)
- [Kotlin Mastermind: 字符串操作](https://kotlinmastermind.com/kotlin-string-functions/)