---
title:    "Java: 读取文本文件"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 为什么

阅读文本文件是编程中常见的操作之一。它可以帮助我们读取并处理大量的数据，节省时间和精力。在这篇博客文章中，我们将学习如何使用Java编程语言来读取文本文件，并深入探讨这个过程背后的原理。

## 如何

在Java中，读取文本文件的最简单方式是使用`FileReader`和`BufferedReader`类。首先，我们需要创建一个指向要读取的文本文件的`File`对象，然后将其传递给`FileReader`构造函数。接下来，我们可以使用`BufferedReader`来读取文件的内容，并将其存储在一个字符串变量中。以下是一个示例代码：

```Java
import java.io.*;

public class ReadTextFile {
    public static void main(String[] args) {
        try {
            // 创建File对象
            File file = new File("myFile.txt");
            
            // 创建FileReader对象
            FileReader fileReader = new FileReader(file);
            
            // 创建BufferedReader对象
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            
            // 读取文件中的内容
            String line = bufferedReader.readLine();
            
            // 打印内容
            System.out.println(line);
            
            // 关闭资源
            bufferedReader.close();
            fileReader.close();
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

假设`myFile.txt`文件中的内容为`Hello, world!`，运行上述代码后，控制台将输出`Hello, world!`。

## 深入探讨

在Java中，文本文件是以字符的形式存储的。因此，我们使用`FileReader`和`BufferedReader`来读取文件时，实际上是按照字符来读取的。`FileReader`读取一个字符的时间比较慢，因此我们使用`BufferedReader`来缓冲读取的字符，从而提高读取效率。

除了使用`FileReader`和`BufferedReader`，我们还可以使用`Scanner`类来读取文本文件。不过，相比之下，`Scanner`类的使用更为复杂，因此在处理大量数据时可能不如`FileReader`和`BufferedReader`效率高。

## 参考资料

- [Java文档 - FileReader](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java文档 - BufferedReader](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java文档 - Scanner](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)

## 参考链接

- [Java文本文件的读取 - 阮一峰的网络日志](http://www.ruanyifeng.com/blog/2019/01/java-io.html)
- [Java文本文件的读取 - 菜鸟教程](https://www.runoob.com/java/java-files-io.html)