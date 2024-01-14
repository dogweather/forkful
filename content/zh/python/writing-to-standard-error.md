---
title:                "Python: 标准错误的编写"
simple_title:         "标准错误的编写"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写入标准错误？

当我们在编写Python程序时，有时候会遇到一些错误，通常我们会使用标准输出来显示错误信息。但是当我们需要将错误信息记录下来，以便进行调试或日志记录时，我们就需要将错误信息写入标准错误。这样可以更方便地跟踪错误并进行后续处理。

## 如何写入标准错误？

要想将错误信息写入标准错误，我们可以使用Python内置的`sys`模块中的`stderr`方法。下面是一个简单的例子：

```Python
import sys

def divide(x, y):
    try:
        result = x / y
    except ZeroDivisionError:
        print("Error: 不允许除数为0", file=sys.stderr)
    else:
        print(f"{x} 除以 {y} 的结果为: {result}")

divide(6, 2)
divide(10, 0)
```

这段代码中，我们定义了一个`divide`函数来进行除法计算，当除数为0时会触发`ZeroDivisionError`，此时我们使用`sys.stderr`打印错误信息，而非使用标准输出。因此，我们可以得到以下输出：

```
6 除以 2 的结果为: 3.0
Error: 不允许除数为0
```

## 深入了解

除了上面提到的方法，我们还可以通过重定向标准错误进行错误信息的记录。这需要借助`sys`模块中的`stderr`和`stdou`对象及`redirect_stderr()`方法。下面我们来看一个例子：

```Python
import sys

def divide(x, y):
    try:
        result = x / y
    except ZeroDivisionError:
        print("Error: 不允许除数为0", file=sys.stderr)
    else:
        print(f"{x} 除以 {y} 的结果为: {result}")

with open("error_log.txt", "w") as f:
    with redirect_stderr(f):
        divide(10, 0)
```

在这个例子中，我们使用`open()`来创建一个`error_log.txt`文件，并通过`redirect_stderr()`方法将标准错误重定向到这个文件中。这样，当程序出现除以0错误时，错误信息就会被记录到`error_log.txt`文件中。

## 参考链接

- [Python sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Python try...except语句](https://www.runoob.com/python3/python3-errors-execptions.html)
- [Python contextlib模块](https://docs.python.org/3/library/contextlib.html#contextlib.redirect_stderr)