---
title:                "阅读文本文件"
date:                  2024-01-20T17:54:41.935273-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
读取文本文件就是将文件内容加载到程序中。程序员这么做是为了处理和分析数据。

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
