---
title:                "Python: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么阅读文本文件

阅读文本文件是学习Python编程中的重要部分。它允许你读取并处理来自不同来源的数据，比如CSV或TXT文件。通过阅读文本文件，你可以轻松地提取所需的信息并对其进行操作。

## 如何阅读文本文件

在Python中，我们使用`open()`函数来打开文本文件。在括号中，我们需要指定文件的路径和打开模式。例如，如果我们想要打开一个名为"data.txt"的文本文件，可以使用以下代码：

```Python
file = open("data.txt", "r")
```

请注意，`"r"`表示以只读模式打开文件。这意味着我们不能对文件进行编辑，只能读取它。如果要写入文件，则需要使用`"w"`。

一旦我们打开了文件，可以使用`read()`方法来读取文件中的所有内容。例如，如果我们想要打印出文件中的所有内容，可以使用以下代码：

```Python
file = open("data.txt", "r")
print(file.read())
```

如果我们想要逐行读取文件并打印每一行的内容，可以使用`readlines()`方法。例如：

```Python
file = open("data.txt", "r")
print(file.readlines())
```

输出可能类似于这样：

```
['Hello World!\n', 'Welcome to my blog post.\n', 'In this post, we will be learning about reading text files in Python.\n']
```

## 深入了解读取文本文件

除了使用`read()`和`readlines()`方法，我们还可以使用`for`循环来逐行读取文本文件。例如，以下代码会打印出每一行的内容：

```Python
file = open("data.txt", "r")
for line in file:
    print(line)
```

还有一些其他有用的方法，如`seek()`和`close()`，可以让我们在读取文件时做一些处理和操作。建议你在学习Python文件操作时更深入地了解这些方法。

## 参考链接

- [Python文档 - 读写文件](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python文件操作指南](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)
- [文件操作的更多方法](https://realpython.com/read-write-files-python/)