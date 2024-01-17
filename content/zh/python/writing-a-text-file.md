---
title:                "编写文本文件"
html_title:           "Python: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
写入文本文件是一种编程技术，用于将数据或文本保存到计算机的硬盘或其他存储设备中。程序员通常这样做是为了保存或备份重要的信息，以便在以后的使用中方便访问。

## 如何：
```Python
with open("myfile.txt", "w") as f:
    f.write("Hello Mandarin readers!")
```
运行上述代码后，您将在相同的目录中找到名为 "myfile.txt" 的文本文件。它将包含一条简单的消息，即 "Hello Mandarin readers!"。您也可以使用不同的模式（如 "a" ）来打开文件以进行追加或读取操作。

## 深入了解：
文本文件写入是一项常见的编程任务，旨在创建永久性文档来存储和检索数据。它通常与读取文本文件的技术相结合，以便完整地操作文件的内容。另外，程序员也可以使用更高级的数据结构，如数据库来存储和管理数据，但文本文件写入仍然是一种经济且可靠的解决方案。在编程中，文本文件的编码类型也很重要，因为它决定了如何显示和处理文本内的特殊字符。

## 参考：
- [Python文档：文本文件的读取和写入](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [维基百科：文本文件](https://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E6%96%87%E4%BB%B6)
- [CSDN：Python文本文件写入介绍](https://blog.csdn.net/eric61016191/article/details/82746878)