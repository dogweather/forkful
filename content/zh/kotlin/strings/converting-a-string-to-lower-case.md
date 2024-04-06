---
date: 2024-01-20 17:38:55.352742-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728\u8BA1\u7B97\u673A\u7684\
  \u65E9\u671F\u5386\u53F2\u4E2D\uFF0C\u5927\u5C0F\u5199\u8F6C\u6362\u6709\u65F6\u5019\
  \u662F\u4E3A\u4E86\u8282\u7701\u5B58\u50A8\u7A7A\u95F4\uFF0C\u56E0\u4E3A\u5927\u5199\
  \u5B57\u6BCD\u8DB3\u591F\u533A\u5206\u4FE1\u606F\u3002\u73B0\u5728\uFF0C\u6211\u4EEC\
  \u6709\u8DB3\u591F\u7684\u5B58\u50A8\u548C\u9AD8\u6548\u7684\u5B57\u7B26\u4E32\u64CD\
  \u4F5C\u65B9\u6CD5\uFF1A 1. \u73B0\u4EE3\u7F16\u7A0B\u8BED\u8A00\u901A\u5E38\u5185\
  \u7F6E\u4E86\u5927\u5C0F\u5199\u8F6C\u6362\u529F\u80FD\uFF0CKotlin \u4E5F\u4E0D\u4F8B\
  \u5916\u3002 2. Kotlin \u4E2D\uFF0C`lowercase()` \u65B9\u6CD5\u91C7\u7528\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.024393-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728\u8BA1\u7B97\u673A\u7684\u65E9\u671F\
  \u5386\u53F2\u4E2D\uFF0C\u5927\u5C0F\u5199\u8F6C\u6362\u6709\u65F6\u5019\u662F\u4E3A\
  \u4E86\u8282\u7701\u5B58\u50A8\u7A7A\u95F4\uFF0C\u56E0\u4E3A\u5927\u5199\u5B57\u6BCD\
  \u8DB3\u591F\u533A\u5206\u4FE1\u606F\u3002\u73B0\u5728\uFF0C\u6211\u4EEC\u6709\u8DB3\
  \u591F\u7684\u5B58\u50A8\u548C\u9AD8\u6548\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u65B9\
  \u6CD5\uFF1A 1."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
