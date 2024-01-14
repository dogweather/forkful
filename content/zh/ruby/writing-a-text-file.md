---
title:                "Ruby: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么要写文本文件？

在编程中，文本文件是一种常用的数据存储方式。通过写文本文件，我们可以保存和管理大量的数据，同时也能与其他程序或人类交互。例如，如果你想要保存一些用户信息，或是记录程序运行过程中的日志，那么写文本文件无疑是一个很好的选择。

## 如何编写文本文件？

编写文本文件的方法有很多种，但以下是最常用的方式：

使用Ruby自带的File类，首先打开一个文件，然后通过write方法写入数据，最后关闭文件。示例如下：

```Ruby
file = File.open("user_info.txt", "w") # 打开文件
file.write("用户名：John\n") # 写入数据
file.write("密码：123456\n")
file.close # 关闭文件
```

执行以上代码后，你会发现当前文件夹下多了一个名为"user_info.txt"的文件，打开后将看到里面的内容就是我们刚才写入的信息。

## 深入了解文本文件

除了上述简单的写入操作，文本文件还有很多高级用法。例如，我们可以使用CSV库来处理CSV格式的文本文件，或是使用YAML库来读写YAML格式的文本文件。此外，对于大型的文本文件，我们还可以使用文件流来实现分块读写，从而提高效率。

## 看看下面这些链接吧！

[File类官方文档](https://ruby-doc.org/core-2.7.3/File.html)

[CSV库官方文档](https://ruby-doc.org/stdlib-2.7.3/libdoc/csv/rdoc/CSV.html)

[YAML库官方文档](https://ruby-doc.org/stdlib-2.7.3/libdoc/yaml/rdoc/YAML.html)