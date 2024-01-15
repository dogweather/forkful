---
title:                "写入标准错误"
html_title:           "Python: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

为什么一个人会使用标准错误来写作？因为标准错误是一种非常有用的方法，可以帮助程序员发现和调试他们的代码中的任何错误。

## 如何

Python中，可以使用`sys`模块中的`stderr`对象来将错误消息写入标准错误流。下面是一个例子：

```Python
import sys

try:
  # 这里是可能会出错的代码
  print("Hello world")

except:
  # 将错误消息写入标准错误
  sys.stderr.write("出错了！")

```

运行上述代码，将会在控制台输出`出错了！`。这是一个非常简单的例子，但它展示了如何使用标准错误来提醒程序员出现了错误。

## 深入探讨

写入标准错误的方法不仅限于在代码中手动添加`sys.stderr.write()`。Python还提供了`logging`模块来帮助程序员记录程序运行过程中的信息。通过设置`level`为`ERROR`，可以将错误消息写入标准错误流中，而且可以通过`format`参数来定制输出格式。这样，程序员就可以更方便地追踪和调试程序中的错误。

## 另请参阅

- [Python官方文档（中文版）](https://docs.python.org/zh-cn/3/tutorial/errors.html)
- [Python异常处理教程](https://www.runoob.com/python/python-exceptions.html)
- [Python logging模块文档](https://docs.python.org/3/library/logging.html)