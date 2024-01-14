---
title:    "Python: 用 Mandarin 写的如果生成随机数"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数？

在编程中，我们经常会遇到需要随机数的情况。随机数可以帮助我们模拟真实世界中的事件和数据，并且在测试代码时可以提供更多的可能性。因此生成随机数是编程中很常见的操作。

## 如何生成随机数？

为了在Python中生成随机数，我们可以使用random模块。首先，我们需要导入random模块，然后使用其中的random()函数来生成0到1之间的随机数。下面是一个示例代码：

```Python
import random

# 生成0到1之间的随机数
random_number = random.random() 

# 打印随机数
print(random_number) 

# 输出示例：0.8250246179509934 
```

如果我们想要生成一个指定范围内的随机整数，可以使用random模块中的randint()函数，以下是一个示例代码：

```Python
import random

# 生成1到10之间的随机整数
random_number = random.randint(1, 10) 

# 打印随机整数
print(random_number) 

# 输出示例：7 
```

## 深入了解生成随机数

随机数生成的结果并不是完全随机的，而是基于一个种子(seed)值。默认情况下，random模块使用系统时间作为种子值，因此每次运行程序都会得到不同的随机数。如果我们想要重复利用相同的随机数序列，可以通过random.seed()来设置种子值，例如：

```Python
import random

# 设置种子值为10
random.seed(10) 

# 生成0到1之间的随机数
random_number = random.random() 

# 打印随机数
print(random_number) 

# 输出示例：0.5714025946899135 

# 生成0到1之间的随机数
random_number = random.random() 

# 打印随机数
print(random_number) 

# 输出示例：0.4288890546751146

# 重置种子值为10
random.seed(10) 

# 生成0到1之间的随机数
random_number = random.random() 

# 打印随机数
print(random_number) 

# 输出示例：0.5714025946899135 
```

除了random模块，我们还可以使用其他的Python包来生成随机数，如numpy包中的random子模块。

## 参考资料

- [Python文档：random模块](https://docs.python.org/3/library/random.html)
- [Python random模块使用指南](https://zhuanlan.zhihu.com/p/52239011)
- [掘金：Python种子不变时的随机数生成](https://juejin.cn/post/6844903992566318605)

## 同类文章

- [如何在Python中使用循环](https://example.com/how-to-use-loops-in-python)
- [学习Python的最佳实践](https://example.com/best-practices-for-learning-python)