---
date: 2024-01-26 00:57:10.233593-07:00
description: "\u5728Python\uFF08\u6216\u4EFB\u4F55\u7F16\u7A0B\u8BED\u8A00\uFF09\u4E2D\
  \u5904\u7406\u9519\u8BEF\uFF0C\u610F\u5473\u7740\u9884\u671F\u5230\u4E0D\u53EF\u9884\
  \u671F\u7684\u60C5\u51B5\u2014\u2014\u5F53\u4EE3\u7801\u51FA\u95EE\u9898\u65F6\uFF0C\
  \u827A\u672F\u6027\u5730\u8FDB\u884C\u7BA1\u7406\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u9632\u6B62\u5D29\u6E83\uFF0C\u6307\u5BFC\u7528\u6237\uFF0C\u4F7F\
  \u6211\u4EEC\u7684\u7A0B\u5E8F\u5065\u58EE\u53EF\u9760\u3002"
lastmod: 2024-02-19 22:05:06.343658
model: gpt-4-1106-preview
summary: "\u5728Python\uFF08\u6216\u4EFB\u4F55\u7F16\u7A0B\u8BED\u8A00\uFF09\u4E2D\
  \u5904\u7406\u9519\u8BEF\uFF0C\u610F\u5473\u7740\u9884\u671F\u5230\u4E0D\u53EF\u9884\
  \u671F\u7684\u60C5\u51B5\u2014\u2014\u5F53\u4EE3\u7801\u51FA\u95EE\u9898\u65F6\uFF0C\
  \u827A\u672F\u6027\u5730\u8FDB\u884C\u7BA1\u7406\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u9632\u6B62\u5D29\u6E83\uFF0C\u6307\u5BFC\u7528\u6237\uFF0C\u4F7F\
  \u6211\u4EEC\u7684\u7A0B\u5E8F\u5065\u58EE\u53EF\u9760\u3002"
title: "\u5904\u7406\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Python（或任何编程语言）中处理错误，意味着预期到不可预期的情况——当代码出问题时，艺术性地进行管理。我们这么做是为了防止崩溃，指导用户，使我们的程序健壮可靠。

## 如何操作：

``` Python
# 基础的try-except块
try:
    # 可能出错的代码
    number = int(input("请输入一个数字："))
except ValueError:
    # 处理错误
    print("那不是一个数字！")

# 指定多个异常
try:
    # 可能引发不同异常的代码
    result = 10 / int(input("请输入一个除数："))
except ZeroDivisionError:
    print("哎呀！不能除以零。")
except ValueError:
    print("我需要一个数字，伙计。")

# 使用else和finally
try:
    number = int(input("请输入一个数字进行平方计算："))
except ValueError:
    print("我说的是数字！")
else:
    # 没有发生错误
    print("你的数字平方是：", number**2)
finally:
    # 总是执行
    print("感谢你尝试这个！")
```

输入无效数字时第一个块的示例输出：
```
请输入一个数字：hello
那不是一个数字！
```

## 深入探讨

自编程诞生之初，错误处理就至关重要。早期的方法很原始，比如在每个可能出错的操作前检查条件。Python的`try-except`语法源于早期语言如C++和Java中的异常处理传统，简化了这一过程。

当你尝试执行一段代码，Python会监视是否有任何异常发生。如果弹出错误，`except`块会捕捉到它。你可以具体化要捕捉的异常，或者用一个空的`except`捕捉所有异常。然而，优先考虑具体化是更好的做法——它精确，而不是用一个大网抓所有的。

`else`和`finally`是这个概念中的额外内容。如果try块没有错误，`else`块会执行。`finally`是可靠的伙伴，无论如何都会运行——想想清理操作。

有别的选择吗？当然有。有些语言使用返回代码而不是异常。你还可能遇到用于处理资源的`with`语句，或者在开发时用来检查条件的`assertions`。但当我们谈到扎实的错误处理策略，基于可读性和结构的try-catch模型突出表现。

## 另请参阅

以下是一些深入研究的好资源：

- Python官方文档关于错误和异常的部分：[Python文档 - 错误和异常](https://docs.python.org/3/tutorial/errors.html)
- Real Python上的指南：[Real Python - try/except/else/finally块](https://realpython.com/python-exceptions/)
- 关于错误处理最佳实践的深入讨论：[Stack Overflow - 如何正确忽略异常？](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
