---
title:                "编写文本文件"
html_title:           "Java: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

为什么会有人写一个文本文件呢？写入文本文件可以将数据保存在一种易于阅读和处理的格式中，这极大地方便了数据的存储和交换。

## 如何

写入文本文件是Java编程中的基础知识，下面通过几个简单的示例来介绍如何实现。

首先，我们需要创建一个名为"myFile.txt"的文本文件，并在其中写入一些文本内容。可以使用Java中的FileWriter类来实现这一步骤：

```Java
FileWriter myFile = new FileWriter("myFile.txt");
myFile.write("这是我写入的第一行文本。");
myFile.write("这是我写入的第二行文本。");
myFile.close();
```

接下来，我们需要读取刚才写入的文本文件内容并打印出来。可以使用Java中的BufferedReader类来实现：

```Java
BufferedReader reader = new BufferedReader(new FileReader("myFile.txt"));
String line = reader.readLine();
while (line != null) {
    System.out.println(line);
    line = reader.readLine();
}
reader.close();
```

这样就可以将文本文件中的每一行内容都打印出来了。除了上述的示例代码，也可以使用其他的类库来实现写入和读取文本文件的功能，例如Apache Commons IO库或者Java的java.nio包。

## 深入了解

除了上面提到的写入和读取文本文件的基础知识，还有一些其他的细节需要注意。

当使用FileWriter类进行写入操作时，可以通过在其构造函数中传入一个布尔值来控制是否覆盖原有的文本文件内容。如果传入的布尔值为false，意味着将在原有文件内容的末尾继续写入内容；如果传入的布尔值为true，则会覆盖原有的文件内容。

另外，当我们使用BufferedReader类进行读取操作时，还可以通过其read方法来设定读取的起始偏移量和读取的长度。这样可以灵活地控制读取文本文件的范围。

## 参考资料

- FileWriter类文档：https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html
- BufferedReader类文档：https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html
- Apache Commons IO库：https://commons.apache.org/proper/commons-io/
- Java NIO包：https://docs.oracle.com/javase/8/docs/api/java/nio/package-summary.html

## 参见

- [如何在Java中读写文件](https://www.liaoxuefeng.com/wiki/1252599548343744/1306041071253666)
- [Java文本文件操作教程](https://www.runoob.com/java/java-files-io.html)