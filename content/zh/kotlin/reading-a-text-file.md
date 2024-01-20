---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？
读取文本文件是一种查看和解析存储在文本文件中的数据的编程技术。程序员需要做这个，以获取所需的信息或进行进一步的数据操作。

## 如何做:
在Kotlin中，我们可以通过“java.io.File”的readText() 函数轻松读取文本文件。以下是一个代码示例:

```Kotlin
import java.io.File

fun main() {
    val data = File("filename.txt").readText()
    println(data)
}
```
在上面的代码中, 你需要用你试图读取的文件名去换掉 `filename.txt`。运行代码, 你将会在输出中看到文件的内容。

## 深入探讨:
读取文本文件的任务虽然简单，但实际上却有着悠久的历史。它是最早的编程任务之一，再早，那就没有电脑了。这些年来，出于使用的便利性和性能的考虑，人们已经发明了多种读取文件的方法。Kotlin的readText()函数就选择了简洁和易用性。但它可能在大文件时会遇到性能瓶颈。一个替代的选择是，使用readLines()，它将读取的内容表示为行的列表，允许逐行处理，减轻内存压力。

```Kotlin
import java.io.File

fun main() {
    val lines = File("filename.txt").readLines()
    lines.forEach { println(it) }
}
```
在上面的代码中, 依旧需要用你试图读取的文件名去换掉 `filename.txt`。这段代码会输出文件中的每一行。

## 另请参阅:
1. [Kotlin 文档 - java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)

注: 如需对文件进行更复杂的操作，比如异步读写、非阻塞IO，可以尝试使用Java的NIO API或者Kotlin的协程。