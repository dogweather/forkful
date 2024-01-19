---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？

生成随机数是一种让计算机输出不确定的值的做法。程序员常用它来进行随机抽样、生成随机的测试数据，或在构建仿真模型时加入随机性元素。

## 如何操作：

在Python中，我们可以使用random库来生成随机数。以下是一些例子：

```Python
import random

# 生成一个介于0.0到1.0之间的随机浮点数
random_float = random.random()
print(random_float)

# 生成一个介于1到10之间的随机整数
random_int = random.randint(1, 10)
print(random_int)

# 从列表中随机选择一个元素
my_list = ['apple', 'banana', 'cherry']
random_choice = random.choice(my_list)
print(random_choice)
```

运行上述代码，您将得到如下的输出（每次运行结果都会不同）：
```Python
0.5387451400628015
9
cherry
```

## 深度解析：

在计算机中生成随机数已经有很长一段历史了，早期的计算机使用物理随机生成器输出随机数。然而，现代计算机主要使用伪随机数生成器，这种算法可以生成看起来像随机数的序列，但实际上是可以预测的。

Python中的random模块就是一个伪随机数生成器。这个模块基于Mersenne Twister算法。除了random之外，还有其他库如NumPy和SciPy也可用于生成随机数。

## 参见：

- Python官方文档：random — 生成伪随机数： https://docs.python.org/3/library/random.html
- Wikipedia：随机数生成器： https://zh.wikipedia.org/wiki/隨機數生成器
- Python中NumPy随机数函数的使用： https://blog.csdn.net/qq_38663737/article/details/80500366