---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

## 什么和为什么? (What & Why?)

创建临时文件是建造一个只有程序运行期间存在的文件。程序员这样做是为了保存短期数据，或者为了数据处理提供个中间阶段。

## 怎么做呢? (How to?)

在Java中，`java.nio.file`包提供了一些创建临时文件的方法：

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile("tempFile", ".txt");
            System.out.println("Temporary file path: " + tempFile.toString());
        } catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}
```
运行这段代码，输出文件路径应该类似于这样：

```Shell
Temporary file path: /tmp/tempFile1234567890.txt
```

## 深挖一下 (Deep Dive)

创建临时文件这个概念已经存在了很长时间，Java从1.2版本就开始支持。通常情况下，临时文件保存在系统的临时目录里（取决于操作系统）。一旦JVM结束或者通过`deleteOnExit()` 设定了自动删除状态，这些临时文件就会被删除。

从Java 7开始，`java.nio.file`包开始提供更简单的方法创建临时文件，并且允许你设置文件属性，例如权限和所有权。`Files.createTempFile`是一个创建临时文件最常用的方法。

如果你需要不同于标准临时文件的行为，例如定位或持久性，你可能需要使用其他技术来创建文件。这取决于你的具体需求和环境。

## 查看更多 (See Also)

- [Oracle官方文档: Files.createTempFile](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#createTempFile-java.nio.file.Path-java.lang.String-java.lang.String-java.nio.file.attribute.FileAttribute...-)
- [创建临时文件 in Java](https://www.baeldung.com/java-create-temporary-file)
- [Java I/O: java.nio.file 文件](https://www.w3cschool.cn/java/java-io-java-nio-file.html)