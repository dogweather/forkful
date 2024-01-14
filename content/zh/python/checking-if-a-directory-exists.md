---
title:                "Python: 检查目录是否存在。"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在进行Python编程时，经常会遇到需要检查某个目录是否存在的情况。这样做可以帮助我们在编写代码时避免一些不必要的错误，并且可以增加程序的可靠性和稳定性。

## 如何操作

我们可以使用os模块中的exists()函数来检查目录是否存在。下面是一个简单的示例代码：

```Python
import os

# 要检查的目录
directory = "documents"

# 使用exists()函数检查目录是否存在
if os.path.exists(directory):
  print("目录已存在")
else:
  print("目录不存在")
```

运行上面的代码，如果存在名为"documents"的目录，就会输出"目录已存在"，否则会输出"目录不存在"。

## 深入了解

除了使用os模块中的exists()函数，我们还可以使用其他一些函数来检查目录是否存在。比如，isdir()函数可以判断是否为一个有效的目录，而isfile()函数则可以判断是否为一个文件。

需要注意的是，使用这些函数前，需要先导入os模块。同时，目录的路径也需要正确指定。

## 参考链接

如果您想了解更多关于Python中检查目录是否存在的知识，可以参考以下链接：

- [os模块官方文档](https://docs.python.org/3/library/os.html)
- [Python中检查文件或目录是否存在的方法](https://www.runoob.com/python3/python3-check-file-directory.html)
- [Python os模块中的exists()函数用法](https://www.jb51.net/article/158425.htm)

## 参考资料

- [Markdown教程](https://www.runoob.com/markdown/md-tutorial.html)
- [如何用Python操作目录和文件](https://houzhaomeng.github.io/blog/python/2019/how-to-handle-dir-and-file-with-python/)