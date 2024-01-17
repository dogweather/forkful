---
title:                "检查目录是否存在"
html_title:           "Python: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么？为什么？

检查目录是否存在是程序员经常使用的一项技术。它可以帮助我们确定一个特定的目录是否存在，并相应地进行操作。这在运行程序时非常重要，因为我们不希望程序出现意外的错误，导致程序终止或产生不正确的结果。

## 如何操作：

```Python
import os # 导入os模块

# 使用os模块中的path.exists()方法来检查目录是否存在
if os.path.exists("my_directory"): 
    print("目录已存在") # 如果目录存在，则打印“目录已存在”
else:
    print("目录不存在") # 如果目录不存在，则打印“目录不存在”

# 还可以使用os模块中的path.isdir()方法来检查是否为一个真实的目录
if os.path.isdir("my_directory"):
    print("是一个真实的目录") # 如果是一个真实的目录，则打印“是一个真实的目录”
else:
    print("不是一个真实的目录") # 如果不是一个真实的目录，则打印“不是一个真实的目录”
```

输出：

```
目录不存在
不是一个真实的目录
```

## 深入了解：

此技术的背景可以追溯到早期的操作系统，当时需要手动创建目录。检查目录是否存在可以帮助程序员节省时间和精力。除了使用os模块外，我们还可以使用try-except的异常处理方法来检查目录是否存在。

## 参考资料：

- 官方Python文档： https://docs.python.org/3/library/os.html
- 更多关于os模块的内容：https://realpython.com/python-pathlib/#check-if-a-file-or-directory-exists