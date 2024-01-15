---
title:                "检查目录是否存在"
html_title:           "Gleam: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

有的时候，在编写程序时需要检查一个文件夹是否存在。这样做是为了提高程序的健壮性，避免在执行某些操作时因为文件夹不存在而导致程序崩溃。

# 如何进行检查

在Gleam中，我们可以使用[`File`](https://gleam.run/core/file.html)模块中的[`exists`](https://gleam.run/core/file.html#functions-exists)函数来检查文件夹是否存在。下面是一个简单的例子：

```Gleam
import gleam/file

let directory = "/Users/username/Documents/test_folder"

// 检查文件夹是否存在
let exists = File.exists(directory)
```

上面的代码首先引入了`gleam/file`模块，然后定义了一个路径为`/Users/username/Documents/test_folder`的文件夹。最后，使用`File.exists`函数来检查该文件夹是否存在，并将返回值赋给`exists`变量。

如果文件夹存在，`exists`变量的值为`true`，否则为`false`。

# 深入了解

在Gleam中，我们可以使用[`File.stat`](https://gleam.run/core/file.html#functions-stat)函数来获取更详细的信息，例如文件夹的权限、大小等。此外，我们还可以通过[`File.list`](https://gleam.run/core/file.html#functions-list)函数来获取文件夹中的文件列表。

记住，在进行任何操作前，都要先检查文件夹是否存在，以避免程序出错！

# 查看详情

了解更多关于Gleam中文件操作的内容，请参阅官方文档：[Gleam File模块](https://gleam.run/core/file.html)。

# 参考链接

- [Gleam官网](https://gleam.run)
- [Gleam文档](https://gleam.run/documentation/)
- [Gleam社区论坛](https://gleam.run/community/)