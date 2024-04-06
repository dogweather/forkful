---
date: 2024-01-20 17:56:52.889845-07:00
description: "How to: \u5982\u4F55\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570 \u547D\
  \u4EE4\u884C\u53C2\u6570\u7684\u4F7F\u7528\u53EF\u4EE5\u8FFD\u6EAF\u5230\u65E9\u671F\
  \u7684\u8BA1\u7B97\u673A\u7CFB\u7EDF\uFF0C\u90A3\u65F6\u7528\u6237\u4E0E\u8BA1\u7B97\
  \u673A\u7684\u4EA4\u4E92\u4E3B\u8981\u662F\u901A\u8FC7\u7EC8\u7AEF\u6765\u5B8C\u6210\
  \u7684\u3002\u5728Python\u4E2D\uFF0C`sys.argv`\u662F\u4E00\u4E2A\u5217\u8868\uFF0C\
  \u5305\u542B\u4E86\u547D\u4EE4\u884C\u8C03\u7528\u65F6\u7684\u6240\u6709\u53C2\u6570\
  \u3002`sys.argv[0]`\u662F\u811A\u672C\u540D\u79F0\uFF0C\u4E4B\u540E\u7684\u5143\u7D20\
  \u662F\u4F20\u9012\u7ED9\u811A\u672C\u7684\u53C2\u6570\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.499537-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570 \u547D\u4EE4\u884C\
  \u53C2\u6570\u7684\u4F7F\u7528\u53EF\u4EE5\u8FFD\u6EAF\u5230\u65E9\u671F\u7684\u8BA1\
  \u7B97\u673A\u7CFB\u7EDF\uFF0C\u90A3\u65F6\u7528\u6237\u4E0E\u8BA1\u7B97\u673A\u7684\
  \u4EA4\u4E92\u4E3B\u8981\u662F\u901A\u8FC7\u7EC8\u7AEF\u6765\u5B8C\u6210\u7684\u3002\
  \u5728Python\u4E2D\uFF0C`sys.argv`\u662F\u4E00\u4E2A\u5217\u8868\uFF0C\u5305\u542B\
  \u4E86\u547D\u4EE4\u884C\u8C03\u7528\u65F6\u7684\u6240\u6709\u53C2\u6570\u3002`sys.argv[0]`\u662F\
  \u811A\u672C\u540D\u79F0\uFF0C\u4E4B\u540E\u7684\u5143\u7D20\u662F\u4F20\u9012\u7ED9\
  \u811A\u672C\u7684\u53C2\u6570\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
