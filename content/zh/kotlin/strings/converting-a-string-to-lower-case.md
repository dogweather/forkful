---
title:                "将字符串转换为小写"
aliases:
- /zh/kotlin/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:55.352742-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (是什么及为什么？)
在编程中，将字符串转换成小写意味着把所有大写字母改为小写版。程序员这样做以实现数据一致性，比如在比较字符串或进行搜索时忽略大小写差异。

## How to: (如何操作：)
```kotlin
fun main() {
    val originalString = "Hello, 你好!"
    val lowerCaseString = originalString.lowercase()
    println(lowerCaseString)  // 输出: "hello, 你好!"
}
```

## Deep Dive (深入了解)
在计算机的早期历史中，大小写转换有时候是为了节省存储空间，因为大写字母足够区分信息。现在，我们有足够的存储和高效的字符串操作方法：

1. 现代编程语言通常内置了大小写转换功能，Kotlin 也不例外。
2. Kotlin 中，`lowercase()` 方法采用 Unicode 标准进行转换，适用于包括中文在内的各种语言。
3. 除了 `lowercase()`，还可以使用 `toLowerCase()` 方法，这是 `lowercase()` 方法在早期版本的 Kotlin 中的名称。

在细节上，转换过程会考虑当前系统的区域设置，但从 Kotlin 1.5 开始，推荐使用不依赖区域的 `lowercase()`。它会处理特殊字符，并令结果与区域无关，从而保证一致性。

## See Also (另请参阅)
- Kotlin 官方文档中的 `String.lowercase()` 方法：[Kotlin String Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Unicode 字符大小写转换介绍：[Unicode Case Folding](https://unicode.org/reports/tr21/)
- 理解字符串比较和区域设置的关系：[Java - String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html) (虽然是 Java 的文档，但对理解 Kotlin 字符串处理也有帮助)
