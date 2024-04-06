---
date: 2024-01-26 00:57:10.233593-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u81EA\u7F16\u7A0B\u8BDE\u751F\u4E4B\u521D\
  \uFF0C\u9519\u8BEF\u5904\u7406\u5C31\u81F3\u5173\u91CD\u8981\u3002\u65E9\u671F\u7684\
  \u65B9\u6CD5\u5F88\u539F\u59CB\uFF0C\u6BD4\u5982\u5728\u6BCF\u4E2A\u53EF\u80FD\u51FA\
  \u9519\u7684\u64CD\u4F5C\u524D\u68C0\u67E5\u6761\u4EF6\u3002Python\u7684`try-except`\u8BED\
  \u6CD5\u6E90\u4E8E\u65E9\u671F\u8BED\u8A00\u5982C++\u548CJava\u4E2D\u7684\u5F02\u5E38\
  \u5904\u7406\u4F20\u7EDF\uFF0C\u7B80\u5316\u4E86\u8FD9\u4E00\u8FC7\u7A0B\u3002\u2026"
lastmod: '2024-04-05T22:51:00.490982-06:00'
model: gpt-4-1106-preview
summary: "\u5F53\u4F60\u5C1D\u8BD5\u6267\u884C\u4E00\u6BB5\u4EE3\u7801\uFF0CPython\u4F1A\
  \u76D1\u89C6\u662F\u5426\u6709\u4EFB\u4F55\u5F02\u5E38\u53D1\u751F\u3002\u5982\u679C\
  \u5F39\u51FA\u9519\u8BEF\uFF0C`except`\u5757\u4F1A\u6355\u6349\u5230\u5B83\u3002\
  \u4F60\u53EF\u4EE5\u5177\u4F53\u5316\u8981\u6355\u6349\u7684\u5F02\u5E38\uFF0C\u6216\
  \u8005\u7528\u4E00\u4E2A\u7A7A\u7684`except`\u6355\u6349\u6240\u6709\u5F02\u5E38\
  \u3002\u7136\u800C\uFF0C\u4F18\u5148\u8003\u8651\u5177\u4F53\u5316\u662F\u66F4\u597D\
  \u7684\u505A\u6CD5\u2014\u2014\u5B83\u7CBE\u786E\uFF0C\u800C\u4E0D\u662F\u7528\u4E00\
  \u4E2A\u5927\u7F51\u6293\u6240\u6709\u7684\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

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
