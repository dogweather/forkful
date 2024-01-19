---
title:                "检查目录是否存在"
html_title:           "Kotlin: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么且为什么？

检查目录是否存在，如其名称所示，指的是验证电脑系统中目录是否存在的过程。程序员做这个检查是因为，在对目录进行读写操作之前，必须确认该目录是否存在，否则可能会出现错误。

## 如何做：

在 Kotlin 中，我们可以使用 File 类的 exists() 方法来检查目录是否存在。以下是示例代码和输出：

 ```Kotlin 
import java.io.File
   
fun main(){
    val directory = File("/path/to/directory")

    if (directory.exists()) {
        println("目录存在")
    } else {
        println("目录不存在")
    }
}
 ```

如果目录存在，输出将为“目录存在”，否则输出为“目录不存在”。

## 深入探究：

检查目录是否存在在文件操作中是很常见的，早期的编程语言如 C 语言也使用类似的操作。

不过，还有其他方法可以检查目录是否存在，如 Java 7 中的 Files.exists() 方法，其在 Kotlin 中也可以使用。另外，Kotlin 的扩展库 kotlinx.io 提供了更为简洁的方法来检查目录是否存在。

实现检查目录是否存在的过程其实很简单，主要是通过系统调用来获取文件状态，再判断该文件是否存在且为目录。

## 另请参阅：

1. [Java 文档：File exists()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
2. [Java 文档：Files exists()](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#exists(java.nio.file.Path,%20java.nio.file.LinkOption...))
3. [Kotlin kotlinx.io 库文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/exists.html)