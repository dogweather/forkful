---
title:    "Kotlin: 创建临时文件"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么 

临时文件是一种在程序中经常使用的文件类型，它们可以帮助我们存储临时性的数据或者临时生成的文件。在Kotlin中，创建临时文件非常简单，让我们来看看如何做到这一点。

## 如何做 

首先，我们需要导入Kotlin的I/O库，这样我们就可以使用它提供的函数来创建临时文件。

```Kotlin
import java.io.File
```

接下来，我们可以使用`createTempFile()`函数来创建一个临时文件。这个函数接收两个参数，第一个是文件名的前缀，第二个是文件名的后缀。我们可以选择性地传入这两个参数，如果我们不指定文件名的前缀或者后缀，Kotlin会自动为我们生成一个随机的文件名。

```Kotlin
// 创建一个没有前缀和后缀的临时文件
val tempFile1 = File.createTempFile()

// 创建一个带有前缀和后缀的临时文件
val tempFile2 = File.createTempFile("prefix-", ".txt")
```

我们可以使用`absolutePath`属性来获取临时文件的绝对路径，使用`deleteOnExit()`方法来告诉JVM在程序结束时删除临时文件。

```Kotlin
println(tempFile1.absolutePath)
tempFile2.deleteOnExit()
```

运行以上代码，你会在控制台看到输出的文件路径。在代码结束后，临时文件`tempFile2`会被自动删除。

## 深入了解 

创建临时文件的过程实际上是在系统的临时目录下创建了一个新的空文件，并且它的权限被设置为允许读写。这样，我们就可以执行各种操作，例如写入数据，读取数据等。

如果我们想要指定临时文件的位置，可以使用`createTempFile()`函数的另一个版本，它接收三个参数：第一个是文件名的前缀，第二个是文件名的后缀，第三个是要创建的目录。

```Kotlin
// 在指定目录下创建一个临时文件
val tempFile3 = File.createTempFile("prefix-", ".txt", File("path/to/directory"))
```

另外，如果我们不想在程序结束后自动删除临时文件，可以调用`delete()`方法来手动删除。

```
// 不删除临时文件
tempFile3.delete()
```

## 参考资料

- [Kotlin官方文档：Java中的临时文件](https://kotlinlang.org/docs/reference/java-interop.html#temporary-files-in-java)
- [Kotlin官方文档：I/O库](https://kotlinlang.org/docs/reference/io.html)
- [Java文档：createTempFile方法](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-io.html)

## 参见 

[Kotlin官方网站](https://kotlinlang.org/)