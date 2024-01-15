---
title:                "编写文本文件"
html_title:           "Python: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

Python是一种流行的编程语言，它可以用于许多不同的任务，包括数据分析、网络开发和自动化。写入文本文件是一种常见的编程任务，它可以帮助您保存和组织数据，并使您的程序更加灵活和可扩展。

## 如何

写入文本文件的基本步骤是打开文件、写入内容并关闭文件。以下是一个简单的例子：

```Python
# 打开文件
file = open("example.txt", "w")

# 写入内容
file.write("这是一个例子文本文件。")

# 关闭文件
file.close()
```

您还可以使用`with`语句来自动关闭文件，例如：

```Python
with open("example.txt", "w") as file:
    file.write("这是一个例子文本文件。")
```

这样可以确保文件在使用完毕后被正确关闭，而无需手动编写`close()`语句。

## 深入探讨

要向文本文件写入多行内容，您可以使用`\n`来表示换行符，例如：

```Python
lines = "这是第一行。\n这是第二行。\n这是第三行。"
with open("example.txt", "w") as file:
    file.write(lines)
```

如果您想要写入其他数据类型，例如数字或列表，您需要先将它们转换为字符串。可以使用`str()`函数进行转换，如下所示：

```Python
num = 123
with open("example.txt", "w") as file:
    file.write(str(num))
```

您还可以使用`format()`方法来格式化字符串输出，例如：

```Python
name = "小明"
age = 20
with open("example.txt", "w") as file:
    file.write("我的名字是{}，我今年{}岁。".format(name, age))
```

有时候，您可能需要在已有的文本文件中添加新的内容，而不是完全覆盖它。这可以通过传递`"a"`参数而不是`"w"`来实现。例如：

```Python
with open("example.txt", "a") as file:
    file.write("这是新添加的内容。")
```

使用`"a"`参数可以在文件末尾继续写入内容，而不会覆盖原来的文本。

## 参考链接

- [Python官方文档](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python教程](https://www.w3schools.com/python/python_file_write.asp)
- [Python文件操作详解](https://www.runoob.com/python/python-files-io.html)
- [用Python写入文件方法总结](https://www.jianshu.com/p/3faf868b55d1)

## 参见