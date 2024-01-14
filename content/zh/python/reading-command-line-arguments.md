---
title:                "Python: 读取命令行参数"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要读取命令行参数

读取命令行参数是一个非常有用的技能，它允许我们通过命令行来控制脚本的执行。这样做能够提高我们的工作效率，也能让我们的脚本更加灵活。

## 如何读取命令行参数

使用Python的 sys 模块可以轻松地读取命令行参数。下面是一个简单的例子：

```Python
import sys

# 打印出输入的命令行参数
print(sys.argv)
```

假设我们在命令行输入 `python script.py arg1 arg2 arg3`，那么上面的代码将打印出 `['script.py', 'arg1', 'arg2', 'arg3']`。我们可以通过 `sys.argv` 的索引来获取不同的参数，比如 `sys.argv[1]` 将会取到 `arg1`。

除了可以获取命令行参数之外，我们还可以使用 optparse 或 argparse 模块来解析和验证命令行参数。

## 深入了解命令行参数

在深入探讨之前，我们先来了解一下什么是命令行参数。命令行参数是指在执行脚本时，在文件名后面跟随的具有特殊含义的字符串，比如上面例子中的 `arg1`、`arg2` 和 `arg3`。它们通常用来控制脚本的行为或传递需要处理的数据。

读取命令行参数有助于我们写出更加灵活的脚本。比如，我们可以通过传递不同的参数来改变脚本的行为，从而实现不同的功能。此外，通过合理地设计命令行参数，我们还可以让我们的脚本更加易用和友好。

# 参考链接

为了深入了解命令行参数的用法，你可以参考以下链接：

- [Python 官方文档 - sys 模块](https://docs.python.org/3/library/sys.html)
- [Python 官方文档 - optparse 模块](https://docs.python.org/3/library/optparse.html)
- [Python 官方文档 - argparse 模块](https://docs.python.org/3/library/argparse.html)