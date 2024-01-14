---
title:                "Python: 读取文本文件"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读文本文件是Python编程中一个重要的技能。通过读取文本文件，我们可以获取数据并对其进行处理和分析。文本文件也是最常见的数据存储格式之一，所以掌握这项技能将会对您的编程能力有所帮助。

## 如何进行

要读取文本文件，您可以使用Python内置的`open()`函数。该函数接受两个参数，第一个是文件名称，第二个是读取模式。例如，如果您要读取名为`data.txt`的文本文件，可以使用以下代码：

```Python
file = open("data.txt", "r") # "r"表示只读模式
```

接下来，您可以使用`read()`函数来读取文件的内容，并将其存储在变量中。例如，如果文件中包含一行文本`Hello World!`，您可以使用以下代码来读取并打印出来：

```Python
file = open("data.txt", "r")
content = file.read()
print(content) # 输出：Hello World!
```

您也可以使用`readline()`函数来逐行读取文件的内容。例如，如果文件有三行文本，分别为`line1`，`line2`，`line3`，使用以下代码可以逐行读取打印出来：

```Python
file = open("data.txt", "r")
line1 = file.readline()
line2 = file.readline()
line3 = file.readline()
print(line1) # 输出：line1
print(line2) # 输出：line2
print(line3) # 输出：line3
```

## 深入了解

在Python中，文本文件是一种迭代器。这意味着您可以使用`for`循环来逐行读取文件的内容，而不需要使用`readline()`函数。例如，您可以使用以下代码来打印出文件的所有内容：

```Python
file = open("data.txt", "r")
for line in file:
    print(line)
```

此外，在使用`open()`函数后，您必须使用`close()`函数来关闭文件，以释放内存资源。为了更便捷地读取文本文件，您也可以使用`with`语句来自动关闭文件。例如：

```Python
with open("data.txt", "r") as file:
    content = file.read()
```

## 参考链接

- [Python读写文件教程](https://www.runoob.com/python3/python3-file-methods.html)
- [Python文本文件迭代器](https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [Python with语句介绍](https://www.programiz.com/python-programming/with-statement)