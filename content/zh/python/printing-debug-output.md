---
title:    "Python: 打印调试输出"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：在程序设计中，调试输出是一个非常重要的步骤，因为它可以帮助我们找出代码中的错误并进行修复。通过打印调试输出，我们可以更加有效地定位到问题所在，并且可以在修复后再次运行程序来验证修复是否成功。

如何做：通过```Python ...```代码块中的实例和输出样例来演示如何打印调试输出。例如：

```Python
# 创建一个测试列表
list = [1, 2, 3, 4, 5]

# 使用循环打印每个元素及其索引
for i in range(len(list)):
    print("在索引{}的位置找到{}的值".format(i, list[i]))
```

输出结果为：

```
在索引0的位置找到1的值
在索引1的位置找到2的值
在索引2的位置找到3的值
在索引3的位置找到4的值
在索引4的位置找到5的值
```

通过打印调试输出，我们可以看到每个元素的值和其对应的索引位置，从而更容易地发现问题所在。

深入了解：当我们需要进行调试时，有时候只是简单地打印一条消息是不够的。我们可以通过在打印语句中使用不同的格式化选项来输出更详细的信息。例如，我们可以使用```%r```来打印变量的原始值，使用```%s```来打印变量的字符串表示，使用```%d```来打印变量的整数表示等等。

此外，我们也可以使用条件语句来决定是否打印调试输出，这样可以避免在正式运行时产生过多的输出信息。例如，我们可以使用以下代码：

```Python
# 创建一个变量
num = 10

# 判断变量是否大于5，若是则打印调试信息
if num > 5:
    print("当前变量的值为：%d" % num)
```

输出结果为：

```
当前变量的值为：10
```

当变量的值大于5时，才会打印调试信息。

参考链接：

- [Python官方文档-字符串格式化](https://docs.python.org/3/library/stdtypes.html#string-formatting)
- [易百教程-Python条件语句](https://www.yiibai.com/python/python_if_else.html)

另请参阅：

[参考链接 1](http://www.runoob.com/python3/python3-debugging.html)

[参考链接 2](https://medium.freecodecamp.org/how-to-debug-small-programs-5ab9c98a5cfd)

[参考链接 3](https://realpython.com/python-debugging-pdb/)