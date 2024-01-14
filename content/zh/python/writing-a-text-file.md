---
title:                "Python: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么要写文本文件

写文本文件是一种常见的程序员行为，因为它可以帮助我们记录信息、存储数据和与用户交互。通过编写文本文件，我们可以在程序运行时动态地读取和写入文件，这有助于我们创建可扩展的程序。

## 如何写文本文件

首先，我们需要通过打开`open()`函数来创建一个文件对象。通过指定文件名和操作模式，我们可以定义如何打开文件。例如，如果我们想在文件末尾添加新行，我们可以使用操作模式`a`。接下来，我们可以使用`write()`函数来写入我们想要的内容，最后使用`close()`函数来关闭文件。

```Python
# 打开文件
file = open("example.txt", "a")

# 写入内容
file.write("Hello, world!")

# 关闭文件
file.close()
```

## 深入了解文本文件的写入

除了简单地写入字符串，我们也可以通过使用`print()`函数来格式化内容并将其写入文件。我们也可以使用换行符`\n`来添加新行。如果我们想要在每次写入后自动添加换行符，可以在`write()`函数中加入`"end='\n'"`参数。此外，我们还可以使用`with`关键词来简化文件打开和关闭的过程。

```Python
# 打开文件并使用`with`关键词
with open("example.txt", "a") as file:
    # 使用`print()`函数来格式化内容并写入文件
    print("Hello, world!", file=file, end="\n")
    # 使用换行符来添加新行
    print("This is a new line.", file=file, end="\n")
```

## 参考链接

- [Python文本文件操作教程](https://www.runoob.com/python/python-files-io.html)
- [Python `open()`函数文档](https://docs.python.org/3/library/functions.html#open)
- [Python `print()`函数文档](https://docs.python.org/3/library/functions.html#print)