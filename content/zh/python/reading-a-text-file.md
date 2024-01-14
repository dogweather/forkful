---
title:    "Python: 读取文本文件"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 为什么会读取文本文件
在Python编程中，读取文本文件是一个非常常见且必要的操作。通过读取文本文件，我们可以获取文本中的数据，从而进行进一步的数据处理和分析。读取文本文件也是了解和掌握Python基础知识的重要部分。

## 如何读取文本文件
下面是一个简单的Python代码示例，展示了如何读取一个名为"sample.txt"的文本文件，并打印文件中的内容：

```Python
file = open("sample.txt", "r")
print(file.read())
```

这段代码首先使用`open()`函数打开一个文本文件，并指定打开模式为"r"，表示只读。然后使用`read()`方法读取文件的全部内容，并使用`print()`函数打印内容到控制台。

文本文件的编码格式可能会有所不同，除了指定模式"r"，我们也可以使用`encoding`参数指定文件的编码格式。例如，如果我们要读取一个UTF-8编码的文本文件，可以使用以下代码：

```Python
file = open("sample.txt", "r", encoding="utf-8")
```

读取特定行数或者从特定位置开始读取文本内容也是可能的。可以使用`readline()`和`readlines()`方法来实现，具体可以参考Python官方文档。

## 深入了解文本文件读取
在Python中，文本文件被视为一个可迭代的对象，这意味着我们可以使用循环来逐行读取文件的内容，并进行相应的处理。例如，我们可以创建一个空列表，将文本文件中的每一行作为一个元素添加到列表中。代码示例如下：

```Python
file = open("sample.txt", "r")
lines = []
for line in file:
    lines.append(line)
```

除了使用`open()`函数，我们也可以使用上下文管理器（`with`语句）来读取文本文件。这种方式可以自动关闭文件，避免资源占用和数据丢失的问题。代码示例如下：

```Python
with open("sample.txt", "r") as file:
    for line in file:
        print(line)
```

最后，如果我们要读取的文本文件过大，可能会占用大量的内存空间。这时，可以使用迭代器（`iter()`函数）来逐行读取文件，从而避免一次性加载全部文件内容。

# 参考链接
- [Python官方文档 - 文件对象](https://docs.python.org/3/tutorial/inputoutput.html#methods-of-file-objects)
- [Python官方文档 - `open()`函数](https://docs.python.org/3/library/functions.html#open)
- [Python官方文档 - 生成器和迭代器](https://docs.python.org/3/tutorial/classes.html#generators)