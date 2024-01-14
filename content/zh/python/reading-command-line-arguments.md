---
title:                "Python: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读命令行参数

阅读命令行参数是一种非常重要的技能，它可以帮助您更有效地编写Python程序。通过阅读命令行参数，您可以在运行程序时传递不同的参数，从而改变程序的表现，使其更具有灵活性和扩展性。

# 如何阅读命令行参数

阅读命令行参数非常简单，只需要使用`sys`模块中的`argv`变量即可。在代码中，您可以通过`sys.argv`来访问所有传递的命令行参数，并根据需要进行相应的处理。下面是一个简单的示例，演示如何打印出所有命令行参数以及程序的文件名：

```python
import sys

print("程序文件名：", sys.argv[0])
print("命令行参数：", sys.argv[1:])
```

运行这段代码，并在命令行中传递一些参数，您将看到以下输出：

```
程序文件名： example.py
命令行参数： ['hello', 'world', '123']
```

这里的`sys.argv[0]`代表程序的文件名，而`sys.argv[1:]`代表所有的命令行参数。

# 深入阅读命令行参数

阅读命令行参数的过程其实非常简单，但是它却能为您的程序带来很大的灵活性。通过合理地设置命令行参数，您可以在同一个程序中实现多种不同的功能，而无需修改代码。当然，在阅读命令行参数时，您需要注意一些细节，比如命令行参数的类型以及传递的顺序等等。

# 参考链接

- [Python官方文档: sys模块](https://docs.python.org/3/library/sys.html)
- [CSDN: Python命令行参数读取](https://blog.csdn.net/wzw1114568423/article/details/51342604)
- [简书: Python命令行参数教程](https://www.jianshu.com/p/a961ac7e2d42)

# 参见

更多关于Python编程的相关内容，请参考以下链接：

- [Python入门教程](https://www.runoob.com/python/python-tutorial.html)
- [Python官方文档](https://docs.python.org/3/)
- [CSDN Python社区](https://blog.csdn.net/weixin_44089559/column/info/35171)