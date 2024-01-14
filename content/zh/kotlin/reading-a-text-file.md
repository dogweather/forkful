---
title:    "Kotlin: 読取文本文件"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

在编写软件时，我们通常需要读取文本文件中的内容。这包括从网页、数据库、日志文件等获取数据。因此，学习如何在Kotlin中读取文本文件将大大提高我们的编程能力。

## 如何

要在Kotlin中读取文本文件，我们可以使用`readText()`函数。这个函数接受文件的路径作为参数，并返回文件中的所有内容作为一个字符串。

```Kotlin
val content = File("text.txt").readText()
```

我们也可以使用`readLines()`函数来逐行读取文本文件，并将每一行存储在一个列表中。

```Kotlin
val lines = File("text.txt").readLines()
```

若要指定字符编码，我们可以使用`charset()`函数。

```Kotlin
val content = File("text.txt").readText(charset = Charsets.UTF_8)
```

## 深入探讨

在读取文本文件时，我们还需要注意处理可能出现的异常。例如，当指定路径的文件不存在时，`readText()`和`readLines()`函数都会抛出一个`FileNotFoundException`异常。为了避免程序崩溃，我们应该使用`try-catch`语句来处理这些异常。

此外，我们也可以在读取文件时指定文件的编码格式，帮助我们正确地读取文件中的内容。Kotlin支持的编码格式包括UTF-8、GBK、GB18030等。

最后，我们还可以通过使用`useLines()`函数来一次读取文本文件的一行，并在读取完成后自动关闭文件。这样可以防止文件在使用后没有被及时关闭，造成资源浪费。

## 参考资料

- [Kotlin官方文档：读写文件](https://www.kotlincn.net/docs/reference/basic-input-output.html#read-write-files)

## 更多学习

- [Kotlin中文社区](https://www.kotlincn.net/)
- [Kotlin官方网站](https://kotlinlang.org/)
- [Kotlin语言入门教程](https://www.runoob.com/kotlin/kotlin-tutorial.html)