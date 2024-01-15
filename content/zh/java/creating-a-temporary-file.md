---
title:                "创建临时文件"
html_title:           "Java: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

有时候在编程中，我们需要临时存储一些数据。临时文件是一种便捷的方法，可以帮助我们在程序执行过程中存储和处理临时数据。接下来，让我们来了解如何在Java中创建临时文件。

## 如何创建临时文件

创建临时文件需要使用Java提供的`File`类和`createTempFile()`方法。在下面的示例中，我们将通过创建一个临时文件来存储“Hello, World！”这个字符串。

```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class CreateTempFile {
    public static void main(String[] args) {
        try {
            // 使用createTempFile()方法创建临时文件，并指定文件名前缀和后缀
            File tempFile = File.createTempFile("hello", ".txt");

            // 创建FileWriter对象，并将数据写入临时文件中
            FileWriter writer = new FileWriter(tempFile);
            writer.write("Hello, World!");
            writer.close();

            // 输出临时文件的路径
            System.out.println("临时文件的路径为：" + tempFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

运行上面的代码，将得到如下输出：
```sh
临时文件的路径为：/var/folders/f7/pj0k0tzj0y7g3hjswkf7fvb40000gn/T/hello4099076926865249373.txt
```

可以看到，一个以`hello`为前缀，`.txt`为后缀的临时文件在指定的临时目录下被创建，并且成功存储了我们传入的字符串。

## 深入了解

在创建临时文件时，我们可以通过指定文件名称的前缀和后缀来自定义文件名。此外，我们也可以指定临时文件的存储路径，而不是使用系统默认的临时目录。

当临时文件不再需要时，我们可以调用`deleteOnExit()`方法来删除临时文件。这样可以确保临时文件在程序结束时被正确清理，避免占用宝贵的系统资源。

最后，值得注意的是，临时文件只应该用于临时存储数据，而不是作为永久的存储解决方案。

## 更多资源

想要了解更多关于Java中临时文件的内容，可以参考以下链接：

- [Java文档 - File类](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Java文档 - File#createTempFile()方法](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String))
- [菜鸟教程 - Java实现临时文件的创建和删除](https://www.runoob.com/java/java-examples.html)
- [JavaGuide - Java中的临时文件](https://github.com/Snailclimb/JavaGuide/blob/master/docs/java/basis/temp-file.md)

## 参见

- [创建临时文件 (Java Tutorial)](https://docs.oracle.com/javase/tutorial/essential/io/file.html#tempfile)