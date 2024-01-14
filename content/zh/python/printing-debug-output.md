---
title:    "Python: 打印调试输出"
keywords: ["Python"]
---

{{< edit_this_page >}}

为什么：
许多程序员都会在编程过程中使用打印调试输出的方法来查找和解决程序中的错误。这是一种快速有效的方法，可以帮助程序员更快地找出问题所在，提高代码的质量和可靠性。

如何：下面将介绍使用Python中print函数来打印调试输出的几种不同方式。

```
Python代码示例1:

num = 5
print("当前num的值为：" + str(num))

输出结果：
当前num的值为：5

Python代码示例2：

name = "John"
age = 25
print("姓名：{}, 年龄：{}".format(name, age))

输出结果：
姓名：John, 年龄：25
```

深入了解：
除了基本的使用方法外，我们还可以通过给print函数传递多个参数来打印更多的调试信息。例如：

```
Python代码示例3：

num1 = 10
num2 = 5
sum = num1 + num2
print("第一个数：{}, 第二个数：{}, 和：{}".format(num1, num2, sum))

输出结果：
第一个数：10, 第二个数：5, 和：15
```

我们也可以通过设置sep和end参数来自定义输出的格式。例如：

```
Python代码示例4：

num1 = 5
num2 = 3
print(num1, num2, sep=" is greater than ", end="!")

输出结果：
5 is greater than 3!
```

总之，打印调试输出是一种非常简单但却非常重要的调试方法，可以帮助我们发现并修复程序中的错误。

另外，我们还可以使用Python中的日志模块来记录调试信息，这种方式更加灵活和方便，可以在需要时开启和关闭，详细了解可以参考[Python官方文档](https://docs.python.org/3.9/library/logging.html)。

查看也可以：

- [Python中print函数的官方文档](https://docs.python.org/3.9/library/functions.html#print)
- [使用Python的日志模块进行调试的方法](https://realpython.com/python-logging/)