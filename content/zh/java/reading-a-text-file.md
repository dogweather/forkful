---
title:                "Java: 读取文本文件"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么阅读文本文件

如果您是一名Java程序员，并且正在努力学习如何处理文本文件，那么您来对地方了！阅读文本文件是Java编程中非常重要的一部分，因为它允许您访问并操作文本数据。当您掌握了文本文件的读取技巧后，您将能够轻松地处理各种类型的文本信息，从而提升您的编程技能。

# 如何阅读文本文件

阅读文本文件的方法相对简单，可以通过使用Java的Input流来实现。首先，我们需要创建一个File对象，指向我们要读取的文本文件。然后，我们可以使用BufferedReader类来读取文本文件的内容，并将其存储在String变量中。最后，我们可以使用While循环来一次读取一行文本，并将其打印出来。示例代码如下所示：

```Java
File file = new File("textfile.txt"); //创建一个指向文本文件的File对象
BufferedReader br = new BufferedReader(new FileReader(file)); //使用BufferedReader类来读取文本文件的内容

String line; //创建一个String变量用于存储每行的文本

while((line = br.readLine()) != null){ //使用While循环来逐行读取文本文件的内容
  System.out.println(line); //打印每行文本
}

br.close(); //关闭BufferedReader
```

该代码的输出结果将会将文本文件的内容一行一行地打印出来，从而达到阅读文本文件的目的。

# 深入了解

阅读文本文件这一主题还有很多深入的内容可以探讨，比如如何处理编码格式不同的文本文件、如何读取特定行或特定字符、如何将文本文件的内容存储到数组或集合等等。如果您想要进一步学习关于阅读文本文件的内容，我们推荐您查阅Java官方文档或相关书籍，以便更加全面地掌握该技能。

# 参考链接

- [Java官方文档](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [《Java核心技术卷一》书籍](https://book.douban.com/subject/26880667/)
- [《Java编程思想》书籍] (https://book.douban.com/subject/2130190/)

# 参考链接