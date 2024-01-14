---
title:    "Python: 读取文本文件"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么读取文本文件？

阅读文本文件是每位程序员都必须掌握的一项基本技能。它可以帮助我们在进行数据分析、文本处理和编程挑战时更加高效和简便。

## 如何读取文本文件

首先，我们需要使用Python内置的open（打开）函数来打开我们想要读取的文本文件。接着，我们可以使用read（读取）方法来读取文件内容并将其存储为变量。最后，我们可以使用print（打印）函数来显示文件内容。下面是一个示例代码：

```Python
file = open("example.txt") # 打开文本文件 example.txt
content = file.read() # 读取文件内容并存储为变量 content
print(content) # 打印文件内容
```

运行上述代码后，我们可以看到文本文件的内容被打印出来。

## 深入了解读取文本文件

除了使用read（读取）方法来读取整个文件，我们还可以使用其他方法来读取特定行或字符。例如，readline（读取一行）方法可以读取文件中的一行内容，而readlines（读取多行）方法可以将文件的每一行作为一个元素存储在一个列表中。此外，我们也可以使用with语句来自动关闭文件，避免出现资源泄露的问题。

## 参考链接

- [Python 官方文档](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [菜鸟教程：Python 文件](https://www.runoob.com/python3/python3-file-methods.html)
- [Python 文档（中文版）：文件读写](https://docs.python.org/zh-cn/3/tutorial/inputoutput.html#reading-and-writing-files)

## 参见

[Python 包管理器：pip](https://www.python.org/dev/peps/pep-0008/)