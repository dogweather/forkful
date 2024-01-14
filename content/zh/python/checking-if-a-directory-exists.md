---
title:    "Python: 判断目录是否存在"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 为什么

## 检查目录是否存在可能意味着什么？
在编写Python程序时，我们经常需要操作文件和目录。有时候，我们需要检查某个目录是否已经存在，才能进行相应的操作。这样可以避免重复创建目录或者执行无意义的操作。因此，通过编写代码来检查目录是否存在是非常重要的一步。

## 怎么做？

```Python
import os #导入os模块
directory = "my_folder" #定义要检查的目录名
if os.path.isdir(directory): #使用os模块的isdir()函数来判断目录是否存在
    print("目录已存在")
else:
    print("目录不存在")
```

输出：
```
目录已存在
```
在上面的代码中，我们首先导入了Python的os模块，它提供了许多与操作系统交互的函数。然后，我们定义了一个变量来存储要检查的目录名称。接着，通过使用isdir()函数来判断目录是否存在，并根据判断结果输出不同的信息。

## 深入了解

如果你想更加灵活地操作目录，还可以使用os模块的其他函数来实现。比如，创建一个新的目录或者删除一个已经存在的目录。你也可以使用try/except语句来处理检查目录是否存在时可能出现的异常情况。除了os模块，Python还有许多其他专门用来操作文件和目录的模块，如shutil和pathlib等，在实际的应用中可以根据需要进行选择。

## 查看也许感兴趣的内容

[如何使用Python创建和操作目录](https://www.runoob.com/python/os-file-methods.html)

[Python官方文档 - os模块](https://docs.python.org/zh-cn/3/library/os.html)

[深入理解Python中常用的文件和目录操作](https://mp.weixin.qq.com/s/y-mBp-AmFadeAXklP7T0NA)