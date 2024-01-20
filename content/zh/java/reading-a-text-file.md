---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

在编程上，“读取文本文件”是指从计算机中的文本文件获取数据。编程人员对文本文件数据的处理和分析是有需求的。

## 如何实施：

Java 提供了多种方法读取文本文件，我将向您展示使用 `java.util.Scanner` 类的一个简单例子。

```Java
try{
    File myFile = new File("demo.txt");
    Scanner myReader = new Scanner(myFile);
    while (myReader.hasNextLine()) {
        String data = myReader.nextLine();
        System.out.println(data);
    }
    myReader.close();
} catch (FileNotFoundException e) {
    System.out.println("文件未找到");
    e.printStackTrace();
}
```
如果 "demo.txt" 中有如下文字：“Hello World”，运行此代码段后控制台会输出：

```
Hello World
```

## 深度解析：

1. 历史背景：Java 从最初的版本就为文件处理提供了支持，包括读取和写入。从 Java 1.5 版开始，引入了 java.util.Scanner 类，大大简化了文本文件的读取操作。

2. 替代方案：除了以上方法，我们还可以使用 BufferedReader、FileReader 或 java.nio.file 类来读取文件。这些类更适用大文件的处理，因为他们的读取效率更高。

3. 实现细节：当我们使用 Scanner 类读取文件时，Java 会开启一个 InputStream 并从中获取数据，直到文本末尾。

## 另请参见：

1. Oracle Java 文档：[使用 java.util.Scanner 读取文本文件](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)

2. StackOverflow 讨论：[Java 中读取文件的多种方法比较](https://stackoverflow.com/questions/326390/how-to-create-a-java-string-from-the-contents-of-a-file)

3.  Java教程：文件处理 [http://www.runoob.com/java/java-files-io.html](http://www.runoob.com/java/java-files-io.html)