---
aliases:
- /zh/kotlin/creating-a-temporary-file/
date: 2024-01-20 17:40:43.744453-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u4E00\u4E2A\u5728\u7CFB\u7EDF\
  \u4E2D\u751F\u6210\u4E34\u65F6\u5B58\u653E\u6570\u636E\u7684\u6587\u4EF6\u7684\u8FC7\
  \u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u56E0\u4E3A\u4ED6\u4EEC\u901A\
  \u5E38\u9700\u8981\u5904\u7406\u4E34\u65F6\u6570\u636E\u6216\u8FDB\u884C\u5B89\u5168\
  \u7684\u6587\u4EF6\u64CD\u4F5C\uFF0C\u907F\u514D\u5E72\u6270\u4E3B\u8981\u7684\u6587\
  \u4EF6\u7CFB\u7EDF\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.112183
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u4E00\u4E2A\u5728\u7CFB\u7EDF\
  \u4E2D\u751F\u6210\u4E34\u65F6\u5B58\u653E\u6570\u636E\u7684\u6587\u4EF6\u7684\u8FC7\
  \u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u56E0\u4E3A\u4ED6\u4EEC\u901A\
  \u5E38\u9700\u8981\u5904\u7406\u4E34\u65F6\u6570\u636E\u6216\u8FDB\u884C\u5B89\u5168\
  \u7684\u6587\u4EF6\u64CD\u4F5C\uFF0C\u907F\u514D\u5E72\u6270\u4E3B\u8981\u7684\u6587\
  \u4EF6\u7CFB\u7EDF\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
创建临时文件是一个在系统中生成临时存放数据的文件的过程。程序员这样做是因为他们通常需要处理临时数据或进行安全的文件操作，避免干扰主要的文件系统。

## How to: 如何操作：
```kotlin
import java.io.File

fun main() {
    // 创建临时文件
    val tempFile = createTempFile(prefix = "example_", suffix = ".tmp")
    
    // 写入临时文件
    tempFile.printWriter().use { out -> 
        out.println("这是一个临时文件内容示例。")
    }
    
    // 打印临时文件位置
    println("临时文件已创建于: ${tempFile.absolutePath}")
    
    // 删除临时文件（通常在最后需要做的事情）
    tempFile.deleteOnExit()
}

// 输出示例：临时文件已创建于: /var/folders/xx/xxx.../example_xxxxxx.tmp
```

## Deep Dive 深入探究
创建临时文件的做法由来已久。在历史上，当内存资源有限时，临时文件被用来保存那些不能或不应长时间保留在内存中的数据。现代操作系统提供了创建临时文件的原生支持，Kotlin 通过 `java.io.File` 类简化了操作。除了 `createTempFile`，你也可以使用 `File.createTempFile` 方法。这些临时文件通常存放在系统的临时目录下，如 `/tmp` 或 `C:\Windows\Temp`，并且操作系统可能会在重启后清理这些文件。关键点是，创建临时文件时，你要确保在不需要时将其删除，以防止资源浪费和潜在的安全风险。

## See Also 相关链接
- [Kotlin Documentation for File I/O](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Java `File` class reference](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
