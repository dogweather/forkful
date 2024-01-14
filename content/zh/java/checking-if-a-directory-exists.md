---
title:                "Java: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么
对于Java程序员来说，检查一个文件夹是否存在是非常常见的任务。这种功能可以帮助我们在程序中自动识别文件夹的存在，以便进行后续的操作。例如，我们可能需要在程序中创建一个新的文件夹，但是如果我们不先检查它是否存在，可能会导致程序出错。

## 如何进行检查
要检查一个文件夹是否存在，我们可以使用Java的File类。首先，我们需要创建一个File对象，并将文件夹的路径作为参数传递给它。然后，我们可以调用对象的exists()方法来检查文件夹是否存在。下面是一个简单的示例代码：
```java
File directory = new File("路径/文件夹名称");
if(directory.exists()){
    System.out.println("文件夹存在。");
}else{
    System.out.println("文件夹不存在。");
}
```
运行以上代码，如果文件夹存在，将会输出"文件夹存在。"，否则将会输出"文件夹不存在。"

## 深入了解
除了检查文件夹是否存在外，我们还可以获得关于文件夹的其他信息，比如它是一个文件夹还是一个文件、文件夹的绝对路径、最后修改的时间等等。这些信息可以通过调用File对象的不同方法来获取。例如，我们可以使用isDirectory()方法来判断一个对象是否为文件夹，使用getPath()方法来获取文件夹的路径。有关File类的更多详细信息，请参阅Java官方文档。

## 参考链接
- [Oracle Java官方文档](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [CSDN博客：Java检查文件是否存在](https://blog.csdn.net/mbjergensen/article/details/2595835)
- [菜鸟教程：Java File类](http://www.runoob.com/java/java-files-io.html)

# 参见
- [Java中文API索引](https://docs.oracle.com/javase/8/docs/technotes/guides/index.html)
- [Java菜鸟教程](http://www.runoob.com/java/java-tutorial.html)