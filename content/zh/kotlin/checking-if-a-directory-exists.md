---
title:                "Kotlin: 检查目录是否存在"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编写Kotlin程序时，有时候需要检查指定目录是否存在。这可以帮助我们确保程序可以正常运行，并避免潜在的错误。

## 如何操作

我们可以使用Kotlin的`java.io.File`类来检查目录是否存在。首先，我们需要创建一个`File`对象，传入目录的路径作为参数。然后，我们可以调用`exists()`方法来检查目录是否存在。

```Kotlin
val directoryPath = "/Documents/photos" //假设这是我们要检查的目录路径
val directory = File(directoryPath) //创建一个File对象
if(directory.exists()) { //检查目录是否存在
    println("目录存在。") //如果存在，打印信息
} else {
    println("目录不存在。") //如果不存在，打印信息
}
```

运行以上代码，输出结果为"目录存在。"或"目录不存在。"，取决于路径所指向的目录是否存在。

## 深入了解

在深入了解检查目录是否存在之前，我们需要先了解一下`java.io.File`类。这个类是Java中用于文件和目录操作的核心类，Kotlin也可以直接使用。通过创建一个`File`对象，我们可以对指定的文件或目录进行操作，包括检查是否存在、创建文件或目录、删除文件或目录等。

对于检查目录是否存在，除了`exists()`方法，我们还可以使用`isDirectory()`方法来判断指定的路径是否是一个目录。

另外，我们也可以使用`mkdir()`方法来创建一个新的目录，如果目录不存在的话。

## 参考链接

- [Kotlin官方文档：Input and Output](https://kotlinlang.org/docs/tutorials/kotlin-for-py/input-and-output.html)
- [Java官方文档：File类](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Kotlin与Java互操作性：使用Java类和库](https://kotlinlang.org/docs/reference/java-interop.html#using-java-classes-and-libraries) 

## 参考

- [Kotlin官方文档：文件操作](https://kotlinlang.org/docs/reference/java-io-file.html)
- [Kotlin中的文件操作](https://my.oschina.net/molun/blog/1585739)
- [使用Kotlin操作目录和文件](https://www.jianshu.com/p/649bae5e10a8)