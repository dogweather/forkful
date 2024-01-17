---
title:                "生成随机数"
html_title:           "Python: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么是随机数 & 为什么要生成它？
随机数是指在一定范围内没有特定模式的数。程序员经常需要生成随机数，因为它们可以用来模拟真实世界的情况，例如抽奖、游戏中的随机事件等。

## 如何生成随机数：
```Python
# 导入 random 模块
import random

# 示例 1：生成 0 到 100 之间的随机整数
random_integer = random.randint(0, 100)
print(random_integer)

# 示例 2：生成 0 到 1 之间的随机小数
random_float = random.random()
print(random_float)

# 示例 3：从指定列表中随机选择一个元素
fruits = ["apple", "banana", "orange"]
random_fruit = random.choice(fruits)
print(random_fruit)
```
输出结果：
```Python
# 示例 1：随机整数（每次结果不同）
52 
# 示例 2：随机小数（每次结果不同）
0.2715 
# 示例 3：随机选择一个水果（每次结果不同）
orange 
```

## 深入了解：
### 历史背景：
在计算机发展的早期，随机数的生成依赖于硬件设备，例如噪声产生器。随着技术的发展，现在我们可以通过软件程序来生成随机数了。

### 其他选择：
除了使用Python的random模块来生成随机数，还可以使用第三方模块，例如Numpy和SciPy，在处理大量数据时更高效。也可以通过调用随机数生成器的API来实现随机数生成。

### 实现细节：
在计算机上，所谓的“随机数”并不是完全随机的，它们是通过随机数生成算法来生成的。这些算法使用伪随机数生成器（PRNG）来产生伪随机数序列。Python的random模块使用的就是一个伪随机数生成器，需要设置种子数来指定生成的随机数序列。种子数相同的情况下，随机数序列会重复。

## 参考资料：
- [Python官方文档 - random模块](https://docs.python.org/3/library/random.html)
- [Python官方文档 - secrets模块（用于生成安全随机数）](https://docs.python.org/3/library/secrets.html)
- [Numpy官方文档 - 随机数生成](https://numpy.org/doc/stable/reference/random/index.html)
- [SciPy官方文档 - 随机数生成及分布](https://docs.scipy.org/doc/scipy/reference/stats.html#random-number-generation-for-generating-random-variables)