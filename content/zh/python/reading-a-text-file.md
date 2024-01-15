---
title:                "读取文本文件"
html_title:           "Python: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

读取文本文件可以让你轻松地获取文本格式的信息并进行处理。作为一名Python程序员，掌握读取文本文件的技巧非常重要，因为文本处理是编程的基础。

## How To

```python
# 使用内置的open()函数打开文本文件，将文件对象赋值给变量file
file = open("sample.txt", "r")

# 使用read()函数读取整个文本文件的内容，并将其赋值给变量text
text = file.read()

# 关闭文件对象
file.close()

# 打印文本文件的内容
print(text)

# 逐行读取文本文件，并打印每一行的内容
file = open("sample.txt", "r")
for line in file:
    print(line)

# 使用with语句来自动关闭文件对象
with open("sample.txt", "r") as file:
    text = file.read()
    print(text)
```

以上是几种常用的读取文本文件的方法，通过这些例子可以看到，首先我们需要使用内置的open()函数来打开文本文件，指定文件的路径和打开的模式（这里使用的是'r'，表示只读）。然后就可以使用read()函数来读取文件的内容，并将其赋值给变量。最后使用close()方法来关闭文件对象，或者使用with语句来自动关闭文件对象。

## Deep Dive

除了上面提及的方法外，Python还提供了许多用于文本文件读取的方法和函数。例如，我们可以使用readline()函数来逐行读取文件的内容，或者使用readlines()函数来将文件的每一行作为列表中的元素。此外，我们还可以使用seek()函数来移动文件的指针位置，以便在特定位置读取文件的内容。

另外，当我们使用open()函数打开文本文件时，还可以指定encoding参数来指定文本文件的编码方式，保证在读取文件内容时可以正确地解码。如果不指定encoding参数，默认使用系统的默认编码方式。

## See Also

- [Python文档：读写文件](https://docs.python.org/zh-cn/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python File Handling (tutorialspoint)](https://www.tutorialspoint.com/python/python_files_io.htm)
- [Python读写文本文件常用的几种方式](https://www.cnblogs.com/lsdb/p/9920499.html)