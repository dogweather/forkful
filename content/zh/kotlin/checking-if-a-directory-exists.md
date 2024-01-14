---
title:    "Kotlin: 检查目录是否存在"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在

在编程中，我们经常遇到需要访问文件或目录的情况。而在访问目录之前，我们需要先确定该目录是否存在。这可以避免在程序运行过程中出现错误，或影响程序的正常执行。

# 如何检查目录是否存在

在Kotlin中，检查目录是否存在可以使用`java.io.File`类中的`exists()`方法。首先，我们需要先创建一个`File`对象，传入要检查的目录路径作为参数。然后，调用`exists()`方法来判断该目录是否存在，如果存在则返回`true`，不存在则返回`false`。

```
Kotlin
val directoryPath = "C:/Users/username/Documents/"
val directory = File(directoryPath)
if (directory.exists()) {
    println("目录存在！")
} else {
    println("目录不存在！")
}

// 输出：
// 目录存在！
```

# 深入了解目录检查

在Kotlin中，`exists()`方法实际上是调用了`File`类中的`canRead()`方法来判断目录是否可读。只有当该目录不为空且拥有读权限时，`exists()`方法才会返回`true`。此外，如果目录路径是一个符号链接，`exists()`方法也会返回`true`。

因此，在使用`exists()`方法来检查目录是否存在时，也需要考虑目录的权限和是否存在符号连接的情况。

## 另外注意

当我们使用相对路径来初始化`File`对象时，`exists()`方法会根据当前工作目录来判断目录是否存在。因此，建议在使用`exists()`方法前，先使用`getAbsolutePath()`方法来获取绝对路径，确保可靠性。

# 参考资料

- [Kotlin官方文档-文件操作](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin官方文档-File.exists()方法](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Java官方文档-File.canRead()方法](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#canRead--)
- [Kotlin官方文档-File.getAbsolutePath()方法](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/get-absolute-path.html)

# 另请参阅

- [Kotlin 文件操作指南](https://www.baeldung.com/kotlin-file)
- [Kotlin简单文件操作教程](https://www.journaldev.com/18686/kotlin-file-io)
- [Kotlin与Java中文件操作的异同](https://stackoverflow.com/questions/30509598/difference-between-kotlin-text-io-and-java-io)