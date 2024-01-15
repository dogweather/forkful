---
title:                "建立临时文件"
html_title:           "Python: 建立临时文件"
simple_title:         "建立临时文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

为什么：创建临时文件对于编程是很有用的，它允许我们临时保存数据，避免占用大量内存或磁盘空间。

当我们需要在代码中暂时存储数据时，临时文件是非常有用的。它允许我们将数据写入文件并在需要时再读取。这也可以帮助我们避免因为数据量过大而导致的程序崩溃。

## How To

要使用Python创建临时文件，我们可以使用内置的`tempfile`模块。首先，我们需要导入这个模块：

```Python
import tempfile
```

然后，我们可以使用`tempfile.NamedTemporaryFile()`方法来创建一个临时文件。这个方法会返回一个`NamedTemporaryFile`对象，我们可以指定`delete=False`来让Python不删除这个临时文件，从而允许我们手动删除它。

```Python
# 创建临时文件
temp_file = tempfile.NamedTemporaryFile(delete=False)

# 将数据写入临时文件
temp_file.write("Hello World!".encode('utf-8'))

# 关闭文件，保存数据
temp_file.close()

# 读取临时文件中的数据
with open(temp_file.name, 'r') as file:
    print(file.read())

# 输出: Hello World!
```

我们也可以使用`with`语句来自动关闭文件，从而避免忘记关闭文件而造成的资源浪费。

```Python
# 使用with语句创建临时文件
with tempfile.NamedTemporaryFile(delete=False) as temp_file:
    # 将数据写入临时文件
    temp_file.write("Hello World!".encode('utf-8'))

    # 读取临时文件中的数据
    print(temp_file.read())

# 输出: Hello World!
```

## Deep Dive

除了`NamedTemporaryFile`方法，`tempfile`模块还提供了其它方法来创建临时文件。比如，我们可以使用`tempfile.TemporaryFile()`方法来创建一个无需命名的临时文件。这个文件会在关闭后自动删除。

```Python
# 创建无需命名的临时文件
temp_file = tempfile.TemporaryFile()

# 将数据写入临时文件
temp_file.write("Hello World!".encode('utf-8'))

# 关闭文件，并且文件会自动删除
temp_file.close()
```

另一个有用的方法是`tempfile.gettempdir()`，它会返回系统上的临时文件夹路径。我们可以使用这个路径来访问和管理系统中的临时文件。

## See Also

更多关于`tempfile`模块的信息，请查看以下链接：

- [Python官方文档 - tempfile模块](https://docs.python.org/3/library/tempfile.html)
- [w3schools - Python tempfile模块教程](https://www.w3schools.com/python/module_tempfile.asp)
- [Python技术论坛 - 创建临时文件的几种方法](https://python.tech.blog/2018/11/29/creating-temporary-files-in-python/)