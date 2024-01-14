---
title:    "Java: 读取文本文件"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么读取文本文件
在Java编程中，读取文本文件是一项非常常见的任务。通过读取文本文件，我们可以获取其中的信息并进行相应的处理。例如，我们可以读取包含学生信息的文本文件，并将其存储在对象中，以便在程序中使用。因此，学习如何读取文本文件是非常有用的。

## 如何读取文本文件
为了读取文本文件，我们需要使用Java中的FileReader和BufferedReader类。首先，我们需要在代码中导入这两个类：
```
import java.io.*;

```
然后，我们可以使用FileReader来打开文本文件，并使用BufferedReader来读取文件中的内容。以下是一个示例代码：
```
FileReader fileReader = new FileReader("students.txt");
BufferedReader bufferedReader = new BufferedReader(fileReader);

String line = bufferedReader.readLine();

while(line != null) {
    // 在此处进行处理，例如将学生信息存储在对象中
    System.out.println(line); // 打印每一行的内容
    line = bufferedReader.readLine(); // 继续读取下一行
}

bufferedReader.close(); // 关闭BufferedReader
```
上述代码首先打开名为“students.txt”的文本文件，并使用BufferedReader来读取文件中的内容。然后，它使用while循环来逐行读取文件，直到读取到文件末尾。在循环中，我们可以对每一行的内容进行相应的处理，例如存储到对象中或打印出来。最后，我们需要关闭BufferedReader来释放资源。

## 深入了解读取文本文件
在Java中，文本文件是通过字节流来读取的。FileReader类是Reader类的子类，用于将字节流转换为字符流，以便我们可以按字符的方式读取文本文件。而BufferedReader类则用来缓存字符，从而提高读取效率。因此，通过组合使用这两个类，我们可以很方便地读取文本文件中的内容。

# 参考链接
- [Java中的FileReader文档](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Java中的BufferedReader文档](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)
- [Java文件读取指南](https://www.tutorialspoint.com/java/java_files_io.htm)
- [开始学习Java编程](https://www.codecademy.com/learn/learn-java)