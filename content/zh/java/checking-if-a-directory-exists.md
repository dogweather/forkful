---
title:                "检查目录是否存在"
html_title:           "Java: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么和为什么？(What & Why?)

检查目录是否存在是在编程过程中检测特定文件夹路径是否存在的操作。程序员之所以需要做这个，是因为在尝试访问、修改或删除特定文件夹时，如果该文件夹不存在，可能会引发错误。

## 如何操作 (How to)

在Java中，我们可以使用`java.nio.file`包中的`Files`类进行目录存在性的检查。示例代码如下：
 
```Java
import java.nio.file.*;

public class Test {
  public static void main(String[] args) {
    Path path = Paths.get("D:/ExampleDirectory");

    if (Files.exists(path)) {
      System.out.println("Directory exists");
    } else {
      System.out.println("Directory doesn't exist");
    }
  }
}
```
在这个例子中，我们先尝试获取一个指向"D:/ExampleDirectory"的`Path`对象，然后使用`Files.exists()`方法检查路径是否存在。如果目录存在，你将看到输出"Directory exists"，否则，会输出"Directory doesn't exist"。

## 深度解析 (Deep Dive)

在Java的早期版本中，我们需要使用 `java.io.File` 类来检查一个目录是否存在，像这样：

```Java
import java.io.File;

public class Test {
  public static void main(String[] args) {
    File dir = new File("D:/ExampleDirectory");

    if (dir.exists()) {
      System.out.println("Directory exists");
    } else {
      System.out.println("Directory doesn't exist");
    }
  }
}
```

这两种方法都可以用来检测目录是否存在，但是使用 `java.nio.file.Path` 和 `java.nio.file.Files` 的好处是它们提供更良好的异常处理和更有效的文件 I/O 操作。

正在考虑其他替代方案时，可以查看 Apache Commons IO 库，它提供了更多有关文件和目录处理的高级功能。

## 推荐阅读 (See Also)

为了更深入了解Java中的文件和目录处理问题，可以参考以下资源:

1. [Oracle官方文档 - The Java Tutorials: Path Operations](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)
2. [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
3. [Java NIO Path by Java Code Geeks](https://www.javacodegeeks.com/2012/09/java-nio-path.html)