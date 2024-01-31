---
title:                "检查目录是否存在"
date:                  2024-01-20T14:57:34.520099-07:00
simple_title:         "检查目录是否存在"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
检查目录是否存在是检查文件系统上特定目录是否已创建的过程。程序员这样做是为了避免错误，比如尝试访问不存在的目录，或者在不重复创建已存在的目录的情况下创建新目录。

## How to: (怎么做：)
在Kotlin中，我们通常使用`java.io.File`类来检查目录是否存在。这里有一个简单的例子:

```kotlin
import java.io.File

fun main() {
    val directory = File("/path/to/directory")

    if (directory.exists() && directory.isDirectory) {
        println("目录存在。")
    } else {
        println("目录不存在。")
    }
}
```

如果目录存在，你会得到：

```
目录存在。
```

如果目录不存在，你会看到：

```
目录不存在。
```

## Deep Dive (深入了解)
在检查目录是否存在这件事背后，有几个值得注意的点。首先，这种做法并不是Kotlin所独有的; 它是从Java继承而来。在更早的编程时代，比如在C语言中，通常需要更多的努力来实现这一功能。

有几种替代方法可以构建相同的功能。例如，`Files`类中的`exists()`方法，它是Java NIO包的一部分，提供了一种更现代的文件操作方式：

```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/path/to/directory")

    if (Files.exists(path)) {
        println("目录存在。")
    } else {
        println("目录不存在。")
    }
}
```

而实现细节来说，当我们调用`exists()`方法时，操作系统会检查文件系统中是否真的有这个目录。这种对文件系统的访问可能会稍微有点慢，特别是在大型或者分布式的系统中。

## See Also (另请参阅)
- [Kotlin Documentation on java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java NIO Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
