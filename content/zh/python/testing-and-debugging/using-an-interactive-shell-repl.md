---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- /zh/python/using-an-interactive-shell-repl/
date:                  2024-01-26T04:17:12.455452-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

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
