---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么？

命令行参数是在启动程序时传递给主函数的参数。程序员进行此操作主要是为了修改程序的运行方式，增强程序的灵活性。

## 如何实现：

Python通过内置的sys模块读取命令行参数。以下是简单的代码示例：

```Python
import sys

def main():
    print("命令行参数是：", sys.argv)

if __name__ == "__main__":
    main()
```

运行这个程序，并加上一些命令行参数：

```
$ python test.py 参数1 参数2
```

输出结果应该是：

```
命令行参数是： ['test.py', '参数1', '参数2']
```

## 深入解析：

历史上，来自Unix的C语言启发了Python的命令行参数处理方式。在C语言中，主函数可以接受命令行参数。然而Python采用的方式更简洁，直接通过sys.argv列表获得参数。

除了直接使用sys模块，还有其他一些方法可以实现类似的功能，比如argparse和getopt模块，它们提供了更多高级功能，如选项解析、帮助信息生成等。

值得注意的一点是，sys.argv[0]是脚本名称，参数从sys.argv[1]开始索引。

## 相关链接：

1. Python sys模块官方文档：https://docs.python.org/3/library/sys.html
2. Python官方教程 - 命令行参数：https://docs.python.org/3/tutorial/stdlib.html#command-line-arguments
3. Python argparse模块官方文档：https://docs.python.org/3/library/argparse.html
4. Python getopt模块官方文档：https://docs.python.org/3/library/getopt.html