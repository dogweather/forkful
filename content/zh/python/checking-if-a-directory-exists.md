---
title:                "Python: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么：

在 Python 编程中，有时候我们需要检查一个特定的文件夹是否存在。这可以帮助我们避免程序出错，同时也能帮助我们更好地管理和组织我们的文件。因此，通过学习如何检查一个文件夹是否存在，可以提高我们的编程技能，让我们的程序更加稳定和高效。

如何：

要检查一个文件夹是否存在，我们可以使用`os`模块中的`path`方法。首先，我们需要导入该模块：

```Python
import os
```

接下来，我们可以使用`os.path.exists()`方法来检查文件夹是否存在，该方法接受一个路径作为参数：

```Python
if os.path.exists("my_folder"):
    print("该文件夹存在")
else:
    print("该文件夹不存在")
```

如果该文件夹存在，则会输出“该文件夹存在”，否则输出“该文件夹不存在”。

深入：

除了使用`os.path.exists()`方法，我们还可以使用`os.path.isdir()`方法来判断一个路径是否为文件夹。该方法也是接受一个路径作为参数，但是会返回一个布尔值，表示该路径是否为文件夹。例如：

```Python
if os.path.isdir("my_folder"):
    print("该路径为文件夹")
else:
    print("该路径不是文件夹")
```
同时，我们也可以使用`os.path.isfile()`方法来判断一个路径是否为文件。

总的来说，通过学习如何检查一个文件夹是否存在，我们不仅可以更好地管理和组织我们的文件，还能提高我们的编程技能。同时，了解相关的`os.path`方法也能帮助我们在处理文件和路径时更加熟练。

另请参阅：

- [Python 官方文档](https://docs.python.org/3/library/os.path.html)
- [如何在 Python 中检查文件是否存在](https://www.datacamp.com/community/tutorials/python-system-commands-os#check)
- [Python 文件和目录操作指南](http://www.runoob.com/python/python-files-io.html)