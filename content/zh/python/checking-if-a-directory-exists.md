---
title:    "Python: 检查目录是否存在"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么
在编程过程中，经常需要检查某个目录是否存在。这可以确保程序正确运行，并且可以避免出现意外的错误。

# 如何
使用Python中的`os.path.exists()`函数可以轻松检查某个目录是否存在。下面是一个简单的示例代码：

```Python
import os

# 定义要检查的目录路径
directory = "/Users/John/Documents"

# 使用os.path.exists()函数来检查目录是否存在
if os.path.exists(directory):
    print("目录存在！")
else:
    print("目录不存在！")
```

输出：

`目录存在！`

# 深入了解
实际上，`os.path.exists()`函数的作用是检查给定的路径是否存在，并且它不仅仅可以用来检查目录，还可以用来检查文件。此外，该函数还可以检查软链接（symlink）和链接（link）是否有效。如果检查的路径是一个目录，则函数会返回`True`，如果路径是一个文件，则会返回`True`，如果路径无效，则返回`False`。因此，当需要检查多种类型的路径是否存在时，`os.path.exists()`函数非常有用。

# 参考资料
- [`os.path.exists()`官方文档（英文）](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [菜鸟教程：Python os.path.exists()方法（中文）](https://www.runoob.com/python/os-path-exists.html)
- [实验楼：Python3 文件与目录检测（中文）](https://www.shiyanlou.com/courses/732/labs/5251/document)

# 参见
- [Python官方文档（中文）](https://docs.python.org/zh-cn/3/)
- [Python教程（中文）](https://www.runoob.com/python/python-tutorial.html)
- [Markdown语法指南（中文）](https://www.runoob.com/markdown/md-tutorial.html)