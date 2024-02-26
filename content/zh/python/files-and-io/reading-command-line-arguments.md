---
date: 2024-01-20 17:56:52.889845-07:00
description: "\u547D\u4EE4\u884C\u53C2\u6570\u8BA9\u6211\u4EEC\u7684\u7A0B\u5E8F\u53EF\
  \u4EE5\u63A5\u6536\u7528\u6237\u5728\u7EC8\u7AEF\u6216\u547D\u4EE4\u63D0\u793A\u7B26\
  \u4E2D\u8F93\u5165\u7684\u4FE1\u606F\u3002\u5229\u7528\u8FD9\u4E9B\u53C2\u6570\uFF0C\
  \u6211\u4EEC\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\u66F4\u52A0\u7075\u6D3B\uFF0C\u6839\u636E\
  \u7528\u6237\u7684\u9700\u6C42\u6267\u884C\u4E0D\u540C\u7684\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:44.904829-07:00'
model: gpt-4-1106-preview
summary: "\u547D\u4EE4\u884C\u53C2\u6570\u8BA9\u6211\u4EEC\u7684\u7A0B\u5E8F\u53EF\
  \u4EE5\u63A5\u6536\u7528\u6237\u5728\u7EC8\u7AEF\u6216\u547D\u4EE4\u63D0\u793A\u7B26\
  \u4E2D\u8F93\u5165\u7684\u4FE1\u606F\u3002\u5229\u7528\u8FD9\u4E9B\u53C2\u6570\uFF0C\
  \u6211\u4EEC\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\u66F4\u52A0\u7075\u6D3B\uFF0C\u6839\u636E\
  \u7528\u6237\u7684\u9700\u6C42\u6267\u884C\u4E0D\u540C\u7684\u4EFB\u52A1\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## What & Why? 什么是命令行参数，以及我们为什么要读取它们？
命令行参数让我们的程序可以接收用户在终端或命令提示符中输入的信息。利用这些参数，我们可以让程序更加灵活，根据用户的需求执行不同的任务。

## How to: 如何读取命令行参数
```Python
import sys

# 打印所有命令行参数
print("命令行参数列表:", sys.argv)

# 使用命令行参数
if len(sys.argv) > 1:
    print("第一个参数:", sys.argv[1])
```

运行示例：
```
$ python your_script.py Hello World
命令行参数列表: ['your_script.py', 'Hello', 'World']
第一个参数: Hello
```

## Deep Dive 深入探讨
命令行参数的使用可以追溯到早期的计算机系统，那时用户与计算机的交互主要是通过终端来完成的。在Python中，`sys.argv`是一个列表，包含了命令行调用时的所有参数。`sys.argv[0]`是脚本名称，之后的元素是传递给脚本的参数。

除了使用`sys`模块，还可以使用其他工具如`argparse`模块，它提供了更丰富的功能来处理命令行参数。使用`argparse`可以定义可选参数和必选参数，还可以自动生成帮助和使用说明。

对于复杂的参数解析，有些程序可能会选择第三方库，比如`click`或者`docopt`。这些库提供了额外的功能和更好的用户体验。

## See Also 相关资源
- Python 官方文档：https://docs.python.org/3/library/sys.html#sys.argv
- argparse 官方文档：https://docs.python.org/3/library/argparse.html
- Click 文档：https://click.palletsprojects.com/en/7.x/
- Docopt：http://docopt.org/
