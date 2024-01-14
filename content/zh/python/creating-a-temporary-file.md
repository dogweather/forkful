---
title:                "Python: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，有时我们需要创建临时文件来保存一些临时数据。这可以帮助我们在程序运行时存储数据，然后在程序结束时删除它们。临时文件也可以用来解决同一个程序的两个进程之间的通信问题。

## 如何

要创建临时文件，我们可以使用Python内置的`tempfile`模块。首先，我们需要导入这个模块：

```Python
import tempfile
```

然后，使用`tempfile`模块的`TemporaryFile`函数来创建一个临时文件对象：

```Python
temp_file = tempfile.TemporaryFile()
```

我们也可以指定使用的模式（`mode`）和`encoding`参数来创建特定类型的临时文件：

```Python
# 创建文本文件模式
temp_text_file = tempfile.TemporaryFile(mode='w+', encoding='utf-8')

# 创建二进制文件模式
temp_binary_file = tempfile.TemporaryFile(mode='w+b')
```

接下来，我们可以像操作普通文件一样操作临时文件，例如写入数据：

```Python
temp_text_file.write("这是临时文件中的数据")
```

最后，记得要关闭临时文件对象以便删除它：

```Python
temp_file.close()
```

## 深入了解

在创建临时文件时，`tempfile`模块会在操作系统的默认临时目录下创建一个临时文件。我们也可以通过指定`dir`和`prefix`参数来控制临时文件的创建位置和名称。

此外，我们还可以使用`NamedTemporaryFile`函数来创建带有指定名称的临时文件，相比`TemporaryFile`函数，它会返回一个打开过的文件对象。

## 参考链接

- [`tempfile`模块官方文档](https://docs.python.org/3/library/tempfile.html)
- [Python官方教程 - 临时文件](https://docs.python.org/3/tutorial/inputoutput.html#temporarily-storing-the-output-of-a-command)
- [Python编程教程 - 创建临时文件](https://realpython.com/python-tempfiles/)