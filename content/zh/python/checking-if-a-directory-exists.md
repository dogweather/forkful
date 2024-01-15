---
title:                "确定目录是否存在"
html_title:           "Python: 确定目录是否存在"
simple_title:         "确定目录是否存在"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

为了保证程序的稳定性和避免出现错误，我们常常需要检查文件夹是否存在。这可以帮助我们在程序运行时做出相应的处理，从而避免不必要的问题。

## 如何实现

通过使用Python中的os模块，我们可以轻松地检查文件夹是否存在。首先，我们需要导入os模块，然后使用os.path.exists()函数来检查文件夹的存在性。

```Python
import os

folder_name = "my_folder"
if os.path.exists(folder_name):
    print("文件夹已存在")
else:
    print("文件夹不存在")
```

运行以上代码，如果my_folder文件夹已经存在，则会输出“文件夹已存在”，否则会输出“文件夹不存在”。

## 深入了解

除了使用os模块，我们还可以使用pathlib模块来检查文件夹是否存在。pathlib模块提供了更加直观的API来处理路径，并且在Python 3.6及以上版本中已经内置，无需额外安装。

```Python
from pathlib import Path

folder_name = "my_folder"
my_folder = Path(folder_name)
if my_folder.is_dir():
    print("文件夹已存在")
else:
    print("文件夹不存在")
```

除了使用is_dir()函数来判断文件夹是否存在，我们还可以使用exists()函数来判断文件或文件夹是否存在。详情请参考Python官方文档中关于pathlib模块的介绍。

## 参考资料

[Python官方文档 - os模块](https://docs.python.org/3/library/os.html)

[Python官方文档 - pathlib模块](https://docs.python.org/3/library/pathlib.html)

[GeeksforGeeks - Check if a File or Directory Exists in Python](https://www.geeksforgeeks.org/python-check-file-directory-exists/)