---
title:    "Python: 读取命令行参数"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##为什么

命令行参数是在Python编程中必须学习的重要概念。它允许我们在程序运行时接收来自用户的输入，从而实现更加灵活和交互式的程序设计。通过学习如何读取命令行参数，您可以为您的程序提供更多的功能，以及让用户可以根据自己的需求来定制程序。

##如何实现

在Python中，我们可以通过`sys`模块中的`argv`方法来读取命令行参数。下面是一个示例代码：

```Python
import sys

# 读取用户输入的命令行参数
args = sys.argv

# 打印出参数列表
print(args)

# 打印出参数的个数
print(len(args))

# 打印出第一个参数
print(args[0])

# 打印出第二个参数
print(args[1])
```

如果您在命令行中运行这段代码，比如`python example.py hello world`，您将会得到以下输出：

```bash
['example.py', 'hello', 'world']
3
example.py
hello
```

##深入了解

在命令行中，用户可以输入多个参数，例如`python example.py hello world`中的`hello`和`world`。这些参数都会被存储在`sys.argv`的列表中。如果用户只输入了命令而没有任何参数，那么`sys.argv`将只包含一个元素，即程序的名称。

另外，您可以使用`argparse`模块来更加灵活地管理和解析命令行参数。这个模块允许您指定参数的名称、类型、默认值以及帮助信息。这样可以帮助您更好地处理用户输入的参数，并且提供更友好的命令行界面。

##另请参阅

- [sys.argv文档](https://docs.python.org/3/library/sys.html#sys.argv)
- [argparse文档](https://docs.python.org/3/library/argparse.html)