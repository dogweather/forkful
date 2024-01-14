---
title:                "Python: 向标准错误写入"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么写标准错误？
标准错误是Python编程中一个很有用的工具。它可以让我们捕获和排查程序中出现的错误，从而帮助我们更好地调试代码。

## 如何做到？
要写入标准错误，我们可以使用Python中的`sys`模块。首先，我们需要导入这个模块：
```Python
import sys
```

然后，我们可以使用`sys.stderr.write()`函数将想要输出的内容写入至标准错误：
```Python
sys.stderr.write("这是一个标准错误示例！")
```

最后，记得要对标准错误进行刷新，以确保内容被正确输出：
```Python
sys.stderr.flush()
```

下面是一个完整的示例程序及其输出：
```Python
import sys

sys.stderr.write("这是一个标准错误示例！")
sys.stderr.flush()
```

输出：
```
这是一个标准错误示例！
```

## 深入了解
除了简单地输出文字，我们也可以使用其他方式来写入标准错误。比如，我们可以使用`print()`函数将内容写入标准错误：
```Python
print("这是一个标准错误示例！", file=sys.stderr)
```

我们也可以将错误信息直接重定向到标准错误，而不用每次都手动写入：
```Python
import sys

# 将所有的错误信息重定向至标准错误
sys.stderr = sys.stdout
```

需要注意的是，写入标准错误的信息是不会被缓冲的，这意味着它们会立即被输出。

## 参考链接
- [Python sys 模块官方文档（英文）](https://docs.python.org/3/library/sys.html)
- [如何使用print函数将内容输出到标准错误（英文）](https://stackoverflow.com/a/3224394)
- [关于标准错误的更多信息（英文）](https://www.journaldev.com/14915/python-print-to-file#python-write-to-stderr)

## 参见
- [如何调试Python程序（中文）](https://www.geeksforgeeks.org/debugging-python-code/)
- [标准输出与标准错误的区别（中文）](https://www.jianshu.com/p/e12931f3f4ee)
- [Python异常处理（中文）](https://www.runoob.com/python/python-exceptions.html)