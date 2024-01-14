---
title:    "Python: 读取命令行参数"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 为什么

如果你是一位喜欢编程的人，你一定知道Python是一门非常流行的编程语言。它简单易学，却也可以完成各种复杂的任务。今天我们要介绍的是Python中如何读取命令行参数，这是一个非常有用的技巧，可以让你更加方便地控制你的程序。阅读这篇文章可以帮助你学习如何在Python程序中使用命令行参数来实现更多的功能。

## 如何做

首先，我们要了解如何在Python中读取命令行参数。我们可以通过sys模块来实现这个功能。下面是一个简单的示例代码，展示了如何读取并打印出命令行参数：

```Python
# 导入sys模块
import sys

# 读取第一个命令行参数（索引为0）
print("第一个参数为：" + sys.argv[0])

# 读取第二个命令行参数（索引为1）
print("第二个参数为：" + sys.argv[1])
```

如果我们在命令行中输入`python script.py argument1 argument2`，那么上面的代码将会输出：

```
第一个参数为：script.py
第二个参数为：argument1
```

通过这个简单的示例，我们可以看到上面的代码可以读取并使用命令行参数。这样一来，我们就可以通过输入不同的参数来控制程序的行为。

## 深入了解

除了上面所提到的sys模块，还有其他一些方法可以实现读取命令行参数的功能。比如，我们也可以使用argparse模块来解析命令行参数，并且可以更加灵活地控制程序的行为。如果你想要更深入地了解有关命令行参数的使用技巧，可以参考下面的链接。

## 参考链接

- [Python官方文档：sys模块](https://docs.python.org/3/library/sys.html)
- [Python官方文档：argparse模块](https://docs.python.org/3/library/argparse.html)
- [阮一峰的博客：Python解析命令行参数](http://www.ruanyifeng.com/blog/2013/06/short_url.html)

# 延伸阅读

如果你对Python感兴趣，可以继续学习其他有关Python的知识。下面是一些推荐的链接，希望可以帮助你更好地学习Python编程。

- [Python官方文档](https://www.python.org/doc/)
- [廖雪峰的Python教程](https://www.liaoxuefeng.com/wiki/1016959663602400)