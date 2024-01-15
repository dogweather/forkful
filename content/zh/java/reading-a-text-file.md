---
title:                "读取文本文件"
html_title:           "Java: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

读取文本文件是我们在Java编程中经常遇到的任务。它可以让我们方便地从外部源获取数据，进而对其进行分析、处理和展示。无论你是初学者还是有经验的Java开发者，学习如何读取文本文件都是非常重要的，因为它可以为你的编程技能增添一项强大的工具。

## 如何做

在Java中，我们可以使用`FileReader`和`BufferedReader`来实现文本文件的读取。首先，我们需要创建一个`File`对象，指定我们要读取的文件路径。然后，使用`FileReader`来创建一个阅读器对象，并将其作为参数传递给`BufferedReader`。最后，我们可以利用`BufferedReader`的`readLine()`方法来逐行读取文件内容，并且将其保存在一个字符串变量中。以下是一个简单的读取文本文件的代码示例：

```Java
import java.io.*;

public class ReadFileExample {

    public static void main(String[] args) {

        // 设置文件路径
        File file = new File("file.txt");

        try {
            // 创建阅读器对象
            FileReader reader = new FileReader(file);
            BufferedReader bufferedReader = new BufferedReader(reader);

            // 逐行读取文件内容
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }

            // 关闭阅读器
            bufferedReader.close();
        } catch (IOException e) {
            System.out.println("发生了错误：" + e.getMessage());
        }
    }
}
```

```bash
# 文本文件内容示例
Hello world!
Welcome to Java programming.
```

当我们运行以上代码时，控制台输出的结果将会是：

```bash
Hello world!
Welcome to Java programming.
```

## 深入探讨

除了基本的文本文件读取外，Java还提供了许多其他的技术来帮助我们更加高效地处理文本文件。例如，`FileWriter`和`BufferedWriter`可以用于向文本文件中写入内容，`RandomAccessFile`可以实现随机访问文件，`Scanner`可以方便地从文本文件中获取特定数据等等。此外，Java还提供了一些实用的API，如`File`、`Path`、`Paths`和`Files`，可以帮助我们更方便地操作文件路径和文件系统。

## 参考资料

- [How to Read a Text File in Java](https://www.baeldung.com/java-read-file)
- [Java File Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Path Documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
- [Java Files Documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)

## 参见

- [深入理解Java中的文件操作](https://github.com/azer89/Read-Write-IO/blob/master/README.md)
- [从Java中的文件读取数据](https://github.com/java-tuts/read-from-file/blob/master/README.md)