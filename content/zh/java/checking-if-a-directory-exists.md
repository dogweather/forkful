---
title:                "检查目录是否存在"
date:                  2024-01-20T14:57:13.833741-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
检查目录是否存在是指确定电脑文件系统中某个特定路径的目录是否已经被创建。程序员这么做是为了避免在目录不存在时执行不适当的操作，比如尝试读取或写入文件，这样可以预防错误和异常。

## How to: (如何执行：)
在Java中，我们可以使用`java.nio.file`包中的`Files`和`Paths`类来检查目录是否存在。

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExistsDemo {
    public static void main(String[] args) {
        Path path = Paths.get("/path/to/directory");

        if (Files.exists(path)) {
            System.out.println("目录存在！");
        } else {
            System.out.println("目录不存在！");
        }
    }
}
```

如果路径指向的目录存在，输出将会是：

```
目录存在！
```

如果不存在，则输出：

```
目录不存在！
```

## Deep Dive (深入了解)
早期Java版本使用`java.io.File`类来检查文件或目录的存在。随着Java 7的推出，`java.nio.file`包提供了一个更灵活和全面的文件I/O操作方式，其中`Files`和`Paths`类是核心部分。通过这些类，可以更简便地进行文件属性检查、目录遍历、符号链接处理等。

除了`Files.exists()`, 还有其他方式来检查目录是否存在。例如，`Files.isDirectory(path)`不仅会告诉我们路径是否存在，还能确定它是否是一个目录。使用`Files.notExists(path)`可以明确地检查路径是否确实不存在，这对于处理文件系统的不确定性很有帮助。

从实现细节上来讲，`Files.exists()`在某些文件系统上可能不是100%准确的，因为文件系统的访问权限问题或其他I/O错误可能导致检查失败。因此，最好的做法是配合异常处理来确保程序的健壮性。

## See Also (参阅)
- [Files.exists() - JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-)
- [Paths (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Paths.html)
- [File (Java Platform SE 7)](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)

当您进一步探索Java文件I/O的世界时，这些资源会提供有价值的信息和示例。
