---
title:                "检查目录是否存在"
date:                  2024-01-20T14:58:13.382528-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
检查目录是否存在是确认电脑上特定文件夹是否已经创建的过程。编程时进行这一操作，可以避免创建文件时的错误，确保数据存储在正确的位置。

## 如何做：
使用Python标准库中的`os`和`pathlib`模块，快速检查目录是否存在。

```Python
import os

# 使用os模块
dir_path = "/path/to/your/directory"
if os.path.isdir(dir_path):
    print("目录存在！")
else:
    print("目录不存在！")

# 使用pathlib模块
from pathlib import Path

dir_path = Path("/path/to/your/directory")
if dir_path.is_dir():
    print("目录存在！")
else:
    print("目录不存在！")
```

## 深入探究：
在Python的早期版本中，检查目录存在通常依赖于`os`模块。`os.path.isdir()`方法是这个任务的老手。但是，在Python 3.4中引入了`pathlib`模块，提供了一个面向对象的文件系统路径库。它更符合现代编程习惯，代码更为优雅。

替代方案还包括使用`os.path.exists()`，虽然它能检查文件和目录的存在，但不区分二者。何时使用哪个，取决于具体需求。

实现这一操作时，底层系统调用会检查文件系统的索引信息，来确认路径指定的目录是否存在。这一操作并不会去读取目录内的文件，因此在大多数情况下执行速度很快。

## 参见：
- Python `os`模块官方文档：https://docs.python.org/3/library/os.html
- Python `pathlib`模块官方文档：https://docs.python.org/3/library/pathlib.html
- 理解Python中的文件与目录操作：https://realpython.com/working-with-files-in-python/
