---
title:                "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么创建临时文件？

当进行一些需要生成临时数据或保存临时文件的操作时，可能会需要创建临时文件。这样做可以避免对真实数据或文件的直接修改，同时也能够方便清理临时内容。

## 如何创建临时文件？

```Kotlin
val tempFile = File.createTempFile("kotlin", ".temp")
println("临时文件名：${tempFile.name}")
```

```kotlin
临时文件名：kotlin8216587011387889281.temp
```

这里使用了`createTempFile()`函数来创建一个临时文件。第一个参数是文件名的前缀，第二个参数是文件名的后缀。临时文件会在系统默认的临时目录中创建，并自动加上随机数字和时间戳作为文件名。

## 深入探讨临时文件的创建

除了使用`createTempFile()`函数，还有其他一些方法可以创建临时文件。例如，可以使用`File()`类和`System.setProperty()`函数来指定临时目录和文件名的前缀和后缀。

临时文件的默认生命周期是随着程序的结束而消失。如果想要在程序运行期间保留临时文件，可以使用`tempFile.deleteOnExit()`函数来设置文件在程序结束时自动删除。

## 参考链接

- [File.createTempFile()文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [如何使用 Kotlin 创建临时文件](https://www.baeldung.com/kotlin-create-temporary-file)