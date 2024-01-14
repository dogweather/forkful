---
title:    "Python: 创建临时文件"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么使用Python创建临时文件

临时文件是程序中常用的一种技术。它们可以用来暂时存储数据，直到程序结束后被自动删除。使用临时文件可以有效地管理和保护数据，同时也可以提高程序的性能。

如何使用Python创建临时文件
```Python
# 导入模块
import tempfile

# 使用TemporaryFile创建临时文件
with tempfile.TemporaryFile() as tmp:
    # 写入数据
    tmp.write(b"Hello World!")
    # 将文件移动到指定目录
    tempfile.TemporaryFile(dir="temp", prefix="test", suffix=".txt")
```

临时文件通常会自动生成一个唯一的文件名，但也可以使用参数来自定义文件名。此外，通过指定`delete=False`参数，可以防止临时文件在程序结束后被自动删除。

深入探讨创建临时文件

在Python中，临时文件可以通过`tempfile`模块中的多种方法来创建，包括`TemporaryFile`、`NamedTemporaryFile`和`mkstemp`函数。

`TemporaryFile`会创建一个匿名的临时文件，自动使用二进制模式，并且在临时文件关闭后会自动删除。`NamedTemporaryFile`会创建一个具有指定名称的临时文件，并且可以选择是否在程序结束后自动删除。`mkstemp`函数会创建一个具有唯一名称的临时文件，并且需要手动关闭和删除。

除了在程序中使用临时文件，它们也可以用于测试和调试代码，比如在测试数据是否正确排序时，可以使用临时文件来输出结果。

另外，临时文件也可以用来在多进程编程中共享数据，从而提高程序的效率。

查看也可以
- [Python文档：tempfile模块](https://docs.python.org/3/library/tempfile.html)
- [Python教程：临时文件的创建和使用](https://realpython.com/python-tempfile/)
- [CSDN博客：Python创建临时文件方法介绍](https://blog.csdn.net/sinat_39950395/article/details/77484779)