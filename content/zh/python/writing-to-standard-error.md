---
title:    "Python: 向标准错误写入"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要写到标准错误

在编写Python程序时，出错是不可避免的。当错误发生时，程序会自动将错误信息打印到标准输出，这样我们可以方便地找到问题所在。但是有时候，我们希望将错误信息打印到标准错误而不是标准输出。这是因为标准错误是一个独立的流，可用于输出错误信息，而标准输出则用于输出正常的程序结果。通过将错误信息打印到标准错误，可以更容易地区分错误信息和正常结果，并更有效地调试和修正程序。

# 如何编写到标准错误

要在Python中编写到标准错误，可以使用sys模块中的stderr对象。首先，要导入sys模块，然后使用```sys.stderr.write()```来输出错误信息。下面是一个简单的示例：

```Python
import sys

num = 1
if num % 2 == 0:
    print("偶数")
else:
    sys.stderr.write("不是偶数")
```

运行这段代码会输出错误信息```不是偶数```到标准错误而不是标准输出。这样就可以清晰地了解到程序出现了什么错误。

# 深入了解写入标准错误

除了上面的示例，还有其他方法可以将错误信息打印到标准错误。比如可以使用logging模块来记录错误信息，或者将错误信息写入一个单独的日志文件。此外，还可以使用try/except语句来捕获异常并将错误信息打印到标准错误。

总的来说，编写到标准错误可以帮助我们更快地发现和解决程序中的错误，提升编程效率。当我们需要调试程序或者查找错误时，可以考虑使用这种方式来输出错误信息。

# 相关阅读

- [Python官方文档：sys模块](https://docs.python.org/3/library/sys.html)
- [Python官方文档：logging模块](https://docs.python.org/3/library/logging.html)
- [Python官方教程：异常处理](https://docs.python.org/3/tutorial/errors.html)