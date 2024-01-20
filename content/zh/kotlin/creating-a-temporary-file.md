---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么和为什么?
创建临时文件是一种编程操作，通过这种操作，程序可以在需要的时候使用这些文件，然后在不需要的时候将其删除。 当程序需要一些短期存在的大量数据存储时，程序员常常会使用这个操作。

## 如何操作
在Kotlin中，可以使用createTempFile函数创建临时文件。 以下是一个简单的例子：
  
```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("tempFile",".txt")
    println("File ${tempFile.path} has been created!")
}
```
在运行这个程序时，它会创建一个新的临时文件，并且将这个文件的路径打印出来。

## 深入研究
创建临时文件的概念可以追溯到早期的计算机系统，当时存储资源非常宝贵且受限。要有效利用这些资源，一种方法就是创建临时的数据存储，而临时文件就是此解决方案的一部分。

在Kotlin中，还有其他的方式可以创建临时文件，比如使用Files类的createTempFile方法。 它的功能和上面使用File类的方法类似。

另外，需要注意的是，虽然临时文件在使用完成后通常会被删除，但是这个删除操作并不是自动进行的。 因此，为了防止资源浪费，最好在程序结束时手动删除这些临时文件。

## 参见
如果你想了解更多关于这个主题的信息，推荐访问以下网站：
- [Oracle官方文档](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#createTempFile-java.nio.file.Path-java.lang.String-java.lang.String-java.nio.file.attribute.FileAttribute...-)
- [StackOverflow: How to create a temporary file in java?](https://stackoverflow.com/questions/617414/create-a-temp-file-in-java)