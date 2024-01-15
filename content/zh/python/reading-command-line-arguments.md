---
title:                "阅读命令行参数"
html_title:           "Python: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么选择阅读命令行参数

阅读命令行参数是Python编程中必不可少的一部分。它允许开发人员在运行程序时通过命令行输入参数，从而为程序提供不同的功能和选项。这有助于开发人员更有效地管理和调试程序，以及更好地满足用户的需求。

## 如何实现

```Python
import sys
```

首先，我们需要导入sys模块来访问命令行参数。接下来，在你的代码中使用```sys.argv```来获取命令行参数的列表，包括程序本身。这个列表可以通过索引来访问各个参数。

```Python
# 假设程序名为hello.py
# 命令行输入 python hello.py World 123
import sys

# 获取命令行参数
arguments = sys.argv

# 访问参数并打印输出
print("Hello " + arguments[1] + "!")
print("Your number is: " + arguments[2])

# 输出：
# Hello World!
# Your number is: 123
```

## 深入了解

除了使用命令行参数作为程序的输入，还可以使用命令行选项来控制程序的行为。例如，使用```-h```选项来显示程序的帮助信息，或者使用```-v```选项来显示程序的版本号。

此外，可以使用第三方模块如Argparse来简化命令行参数的处理，提供更复杂的命令行选项和错误处理功能。

# 参考链接

- [Python官方文档: sys模块](https://docs.python.org/3/library/sys.html)
- [Python官方文档: argparse模块](https://docs.python.org/3/library/argparse.html)
- [Argparse Tutorial](https://docs.python.org/3/howto/argparse.html) (英文)