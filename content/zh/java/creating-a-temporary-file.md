---
title:                "Java: 创建临时文件"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么：

在Java编程中，临时文件是一种非常有用的工具。它可以用来存储临时的数据或者执行一些操作。当程序结束时，临时文件也会被自动删除，从而避免了在系统中留下无用的文件。

## 怎么做

在Java中，创建临时文件可以通过使用`File`类的`createTempFile`方法来实现。下面是一个简单的示例代码：

```Java
File tempFile = File.createTempFile("temp", ".txt");
System.out.println("临时文件的路径：" + tempFile.getAbsolutePath());
```

这段代码会在系统的默认临时文件夹中创建以`temp`开头，以`.txt`结尾的临时文件，并将该文件的路径输出到控制台上。

如果需要指定临时文件的路径和文件名，可以使用另一个重载的`createTempFile`方法：

```Java
File tempFile = File.createTempFile("temp", ".txt", new File("C:\\temp"));
System.out.println("临时文件的路径：" + tempFile.getAbsolutePath());
```

这段代码会在`C:\temp`目录下创建临时文件。

除了创建临时文件，还可以使用`deleteOnExit`方法来设置在程序结束时删除临时文件，从而避免文件的残留。代码示例如下：

```Java
tempFile.deleteOnExit();
```

## 深入了解

创建临时文件时，系统会根据一个特定的前缀和后缀来命名文件。这些前缀和后缀都是由系统默认的前缀和后缀组成的。在Windows系统中，前缀为`java`，后缀为`.tmp`或者`.temp`；在Linux系统中，前缀为`tmp`，后缀为随机字符串。这些默认的前缀和后缀都可以手动修改，但是需要注意不同系统中可能会有不同的限制。

此外，如果希望临时文件的名称是可读性高的，可以使用`renameTo`方法来重命名临时文件。代码示例如下：

```Java
tempFile.renameTo(new File("readable_name.txt"));
```

## 参考链接

- [Java官方文档：File类](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [CSDN博客：Java创建临时文件](https://blog.csdn.net/asd12345678912345678/article/details/105758357)