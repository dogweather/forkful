---
title:                "检查目录是否存在"
html_title:           "Kotlin: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么和为什么？

检查目录是否存在是计算机编程中的一个基本操作，我们需要查询计算机文件系统看是否有特定的目录。这是重要的，因为在我们创建，读取，删除文件或者其他对目录的操作之前，我们需要确定目录是否存在，避免程序出错。

## 怎么做：

Python内置的`os`模块有一个方法`os.path.exists()`可以用来检查目录是否存在。看下面的示范：

```Python
import os

dir_path = '/path/to/directory'

if os.path.exists(dir_path):
    print("Directory exists")
else:
    print("Directory does not exist")
```

如果所指的目录存在，程序会打印 "Directory exists"。如果不存在，它会打印 "Directory does not exist"。

## 深度剖析：

在早期的Python版本中，我们没有`os.path.exists()`这样便捷的函数。我们需要尝试访问目录，然后处理这个过程中可能出现的异常。可以说，`os.path.exists()`函数的出现大大简化了检查目录存在性的过程。

关于检查目录是否存在的替代方法，我们可以使用`os.path.isdir()`函数。这个函数不仅检查路径是否存在，还检查这个路径是否是一个目录。

```Python
import os

dir_path = '/path/to/directory'

if os.path.isdir(dir_path):
    print("Directory exists")
else:
    print("Directory does not exist")
```

`os.path.exists()`和`os.path.isdir()`函数的工作原理都是通过查询操作系统来获得信息。这意味着其速度与你的操作系统和硬件有关。

## 参见：

* Python `os` 模块官方文档：https://docs.python.org/3/library/os.html
* 更深入的`os.path`模块教程：https://realpython.com/python-path/