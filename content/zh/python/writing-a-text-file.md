---
title:    "Python: 编写文本文件"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 为什么要写文本文件

写程序的过程中，有时候需要将数据保存到文件中。文本文件是一种常用的文件类型，它可以保存不同格式的数据，方便程序读取和使用。因此，掌握写文本文件的技巧对于编写有效的程序至关重要。

## 如何写文本文件

Python中有几种方法可以写文本文件。下面是一个简单的例子，展示了如何使用Python将数据写入文本文件。

```Python
# 创建一个存储数据的列表
data = ['苹果', '香蕉', '橘子']

# 打开一个文本文件来写入数据
file = open('水果.txt', 'w')

# 写入数据到文件中，每个元素占一行
for fruit in data:
    file.write(fruit + '\n')

# 关闭文件
file.close()

# 打开文件来读取数据
file = open('水果.txt', 'r')

# 逐行打印文件中的数据
for line in file:
    print(line)

# 关闭文件
file.close()

# 输出:
# 苹果
# 香蕉
# 橘子
```

## 深入探讨写文本文件

除了使用`open()`函数写入文本文件外，在Python中还有一些其他的方法，例如`with`语句和`write()`函数。添加`with`语句可以自动管理文件的打开和关闭，`write()`函数可以写入特定的数据类型，而不仅限于字符串。这些技巧可以提高程序的效率和可读性，请在自己的项目中尝试使用。

# 参考链接

- [Python文档 - 文件操作](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [TutorialPoint - Python文件操作](https://www.tutorialspoint.com/python/python_files_io.htm)
- [Python程序开发入门教程](https://www.runoob.com/python/python-tutorial.html)