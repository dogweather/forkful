---
date: 2024-01-20 17:54:41.935273-07:00
description: "How to: \u8BFB\u53D6\u6587\u4EF6\u5728\u7F16\u7A0B\u7684\u5386\u53F2\
  \u4E2D\u4E00\u76F4\u5F88\u91CD\u8981\u3002\u6700\u65E9\u7684\u65F6\u5019\uFF0C\u6587\
  \u4EF6\u8BFB\u5199\u662F\u76F4\u63A5\u4F7F\u7528\u64CD\u4F5C\u7CFB\u7EDF\u7684\u7CFB\
  \u7EDF\u8C03\u7528\u3002\u968F\u7740\u7F16\u7A0B\u8BED\u8A00\u7684\u53D1\u5C55\uFF0C\
  \u8BED\u8A00\u81EA\u8EAB\u63D0\u4F9B\u4E86\u66F4\u7B80\u5355\u7684\u65B9\u6CD5\u3002\
  \u5728 Kotlin \u4E2D\uFF0C\u6709\u591A\u79CD\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\
  \u7684\u65B9\u5F0F\uFF0C\u4F7F\u7528 `Files.readString`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.947179-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u4EF6\u5728\u7F16\u7A0B\u7684\u5386\u53F2\u4E2D\u4E00\
  \u76F4\u5F88\u91CD\u8981\u3002\u6700\u65E9\u7684\u65F6\u5019\uFF0C\u6587\u4EF6\u8BFB\
  \u5199\u662F\u76F4\u63A5\u4F7F\u7528\u64CD\u4F5C\u7CFB\u7EDF\u7684\u7CFB\u7EDF\u8C03\
  \u7528\u3002\u968F\u7740\u7F16\u7A0B\u8BED\u8A00\u7684\u53D1\u5C55\uFF0C\u8BED\u8A00\
  \u81EA\u8EAB\u63D0\u4F9B\u4E86\u66F4\u7B80\u5355\u7684\u65B9\u6CD5\u3002\u5728 Kotlin\
  \ \u4E2D\uFF0C\u6709\u591A\u79CD\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u7684\u65B9\
  \u5F0F\uFF0C\u4F7F\u7528 `Files.readString` \u662F\u5176\u4E2D\u6700\u76F4\u63A5\
  \u7684\u4E00\u79CD\u3002\u4E0D\u8FC7\uFF0C\u5982\u679C\u5904\u7406\u5927\u6587\u4EF6\
  \u6216\u9700\u8981\u66F4\u9AD8\u7EA7\u7684\u63A7\u5236\uFF0C\u53EF\u80FD\u4F1A\u7528\
  \u5230 `bufferedReader`\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528 `bufferedReader` \u7684\
  \u793A\u4F8B."
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to:
Kotlin 读取文本文件非常直接。下面是个简短示例。

```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("example.txt")
    val fileContent = Files.readString(path)
    println(fileContent)
}
```

如果 `example.txt` 的内容是:

```
Hello, World!
这是一个文本文件示例。
```

运行程序的输出就会是:

```
Hello, World!
这是一个文本文件示例。
```

## Deep Dive
读取文件在编程的历史中一直很重要。最早的时候，文件读写是直接使用操作系统的系统调用。随着编程语言的发展，语言自身提供了更简单的方法。在 Kotlin 中，有多种读取文本文件的方式，使用 `Files.readString` 是其中最直接的一种。不过，如果处理大文件或需要更高级的控制，可能会用到 `bufferedReader`。以下是使用 `bufferedReader` 的示例:

```kotlin
import java.io.File

fun main() {
    val bufferedReader = File("example.txt").bufferedReader()
    val text: List<String> = bufferedReader.readLines()
    bufferedReader.close()
    
    text.forEach { println(it) }
}
```

这种方法对内存管理更友好，尤其是处理大文件时。

## See Also
- [Oracle's Java Tutorials for Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
