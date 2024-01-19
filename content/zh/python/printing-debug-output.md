---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

打印调试输出是开发人员在程序中插入代码，以便在运行时显示关于程序状态和行为的信息。程序员之所以这样做，是因为它帮助调试程序并识别错误。

## 怎么做?

在Python中，我们可以使用 `print()` 函数或 `logging` 模块来打印调试消息。这里有一些例子。

```Python
# 使用 print() 函数
def debug_example():
    for i in range(5):
        print(f"现在的数字是 {i}")
        if i == 3:
            print("找到数字3！")

debug_example()
```

执行这段代码后，输出将类似：

```
现在的数字是 0
现在的数字是 1
现在的数字是 2
现在的数字是 3
找到数字3！
现在的数字是 4
```

我们也可以使用Python的 `logging` 模块更密切瞄准调试信息。

```Python
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

def debug_example_logging():
    for i in range(5):
        logger.debug(f"现在的数字是 {i}")
        if i == 3:
            logger.info("找到数字3！")

debug_example_logging()
```

## 深入探究

打印调试输出的历史可以追溯到计算机编程的早期。过去，开发人员阅读打印的程序列表或机器码跟踪错误。

虽然打印调试是一种常见的解决问题的方法，但并非唯一的选择。例如，有些开发者选择使用IDEs的内建调试工具，或用像 `pdb` 这样Python内建的调试模块。

关于实现，Python的 `print` 和 `logging` 函数处理打印到控制台的所有底层细节，使得开发者可以专注于编程而不必深入了解打印和日志管理的具体操作。

## 参见

1. Python官方文档: Print Function - [点击这里](https://docs.python.org/3/library/functions.html#print)
2. Python官方文档: Logging - [点击这里](https://docs.python.org/3/library/logging.html)
3. Python pdb 调试工具 - [点击这里](https://docs.python.org/3/library/pdb.html)
4. 对于编程调试的历史，参见 Peter J. Bentley的文章, "The Evolution of Debugging" - [点击这里](https://www.researchgate.net/publication/220413757_The_Evolution_of_Debugging)