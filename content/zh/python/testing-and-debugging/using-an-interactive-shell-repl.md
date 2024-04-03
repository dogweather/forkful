---
date: 2024-01-26 04:17:12.455452-07:00
description: "REPL\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u662F\u4E00\u79CD\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u6536\u5355\u4E2A\u7528\
  \u6237\u8F93\u5165\uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\
  \u56DE\u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u5FEB\
  \u901F\u6D4B\u8BD5\u3001\u5B66\u4E60\u3001\u8C03\u8BD5\u6216\u5373\u65F6\u8BA1\u7B97\
  \u3002"
lastmod: '2024-03-13T22:44:47.257448-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u662F\u4E00\u79CD\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u6536\u5355\u4E2A\u7528\
  \u6237\u8F93\u5165\uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\
  \u56DE\u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u5FEB\
  \u901F\u6D4B\u8BD5\u3001\u5B66\u4E60\u3001\u8C03\u8BD5\u6216\u5373\u65F6\u8BA1\u7B97\
  \u3002."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 什么 & 为什么？
REPL，或读取-求值-打印循环，是一种编程环境，它接收单个用户输入，执行它们，并将结果返回给用户。程序员使用它进行快速测试、学习、调试或即时计算。

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
