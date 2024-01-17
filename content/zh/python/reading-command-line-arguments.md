---
title:                "读取命令行参数"
html_title:           "Python: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
读取命令行参数是指将程序运行时输入的文本参数传递给程序。程序员之所以这样做，是因为这样能够让程序更加灵活和可重复使用。

## 如何进行：
下面的代码示例将展示如何读取并打印命令行参数：
```Python
import sys

args = sys.argv
print(args)
```
运行程序时，输入以下命令：
```
python read_args.py arg1 arg2
```
输出结果为：
```
['read_args.py', 'arg1', 'arg2']
```

## 深入探讨：
在过去，程序员通常通过手动解析命令行参数来读取它们。但是现在，有许多Python库可以帮助程序员更轻松地读取和处理命令行参数。除了使用sys.argv之外，也可以使用argparse库来创建更复杂的命令行界面。实际上，在开发复杂的命令行工具时，使用argparse是一个更好的选择。

## 参考链接：
- [Python sys.argv 文档](https://docs.python.org/3/library/sys.html#sys.argv)
- [argparse Python文档](https://docs.python.org/3/library/argparse.html)