---
date: 2024-01-26 04:17:12.455452-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u901A\u8FC7\u5728\u547D\u4EE4\u884C\u4E2D\
  \u8F93\u5165 `python`\uFF0C\u7ACB\u5373\u8FDB\u5165 Python \u7684 REPL\u3002\u4E00\
  \u65E6\u8FDB\u5165\uFF0C\u6D4B\u8BD5\u7B80\u5355\u64CD\u4F5C\u6216\u591A\u884C\u4EE3\
  \u7801\uFF1A."
lastmod: '2024-04-05T21:53:47.611498-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
通过在命令行中输入 `python`，立即进入 Python 的 REPL。一旦进入，测试简单操作或多行代码：

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

实验函数和即时反馈：

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

使用库并实时探索它们的功能：

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

使用快速 `exit()` 或 `Ctrl+D`（在 Windows 上有时是 `Ctrl+Z`）退出。

## 深入探索
REPL 的概念并不唯一于 Python；它和 Lisp 一样古老。许多语言为代码提供了这种即时、互动的环境，以便于动手实践。Python 原生 shell 的替代品包括 IPython 和 Jupyter Notebook，它们提供了增强的互动性、更多功能以及与其他工具更好的集成。Python 的标准 REPL 虽然简单，但它嵌入了 Python 的全部功能，能够处理复杂对象和多线程程序，尽管它缺乏更高级工具中存在的如自动补全和语法高亮等功能。

## 另请参见
- [Python 官方文档中关于解释器的部分](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython：一个高级 Python shell](https://ipython.org/)
- [Jupyter 项目](https://jupyter.org/)
