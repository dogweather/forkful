---
title:                "Kotlin: 创建临时文件。"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么创建临时文件

在编写Kotlin程序时，有时候会需要在程序运行过程中创建临时文件。这种情况下，我们通常需要以一种灵活的方式来处理临时的数据，而临时文件则可以提供这样的功能。临时文件的创建可以让我们在不影响原始数据的情况下，在程序的执行过程中读取、写入和处理临时数据。

# 如何创建临时文件

要创建一个临时文件，我们可以使用 `createTempFile()` 函数。该函数可以接受两个参数，第一个参数是文件名前缀，第二个参数是文件名后缀。下面是一个简单的示例，展示了如何创建一个名为 "temp-file" 的临时文件，后缀为 ".txt"：

```Kotlin
val tempFile = createTempFile("temp-file", ".txt")
```

当我们执行完上述代码后，会在程序运行的临时文件夹中创建一个名为 "temp-fileXXXXXX.txt" 的文件，其中 "XXXXXX" 是随机生成的6位数。

我们也可以指定临时文件需要存放的文件夹，只需要在 `createTempFile()` 函数的第三个参数中指定即可。

除了创建临时文件，我们也可以使用 `createTempDirectory()` 函数来创建临时文件夹。

# 深入探讨创建临时文件

创建临时文件的过程实际上是在操作系统级别创建了一个空文件，并将其标记为 "临时" 文件。在程序执行结束后，系统会自动删除这些临时文件和文件夹，以确保不会占用系统的资源。

我们也可以手动删除临时文件或文件夹，只需要调用 `delete()` 函数即可。另外，在特殊情况下，我们可以使用 `deleteOnExit()` 函数来设置临时文件或文件夹在程序结束后自动删除，这也是默认的设置。

# 参考链接

- [Kotlin官方文档：文件处理](https://www.kotlinlang.org/docs/reference/native/java-files.html)
- [Kotlin官方文档：临时文件和文件夹](https://kotlinlang.org/docs/tutorials/java-interop.html#temporary-files-and-directories)
- [Kotlin中文网：创建临时文件和文件夹](https://www.kotlincn.net/docs/reference/native/java-files.html#%E4%B8%B4%E6%97%B6%E6%96%87%E4%BB%B6%E5%92%8C%E7%9B%AE%E5%BD%95)

# 参见

- [Kotlin官方文档：文件操作](https://kotlinlang.org/docs/reference/native/file-system.html)
- [Kotlin官方文档：标记文件为临时文件](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/delete-on-exit.html)
- [开源中国：Kotlin教程](https://kotlin.oschina.io/docs/reference/java-interop.html#java-io-classes)
- [中国大学MOOC：Kotlin基础课程](https://www.icourse163.org/course/PKU-1002536002)