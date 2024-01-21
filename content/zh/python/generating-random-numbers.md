---
title:                "生成随机数"
date:                  2024-01-20T17:50:11.295893-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
在Python中生成随机数意味着让计算机产生接近于随机的数字，用于测试、数据分析等。程序员会用到它们是因为很多情形下我们需要数据，但又不依赖特定的数据。

## How to: 如何做？
```Python
import random

# 生成一个0到10之间的随机整数
random_integer = random.randint(0, 10)
print(random_integer)

# 生成一个0到1之间的随机浮点数
random_float = random.random()
print(random_float)

# 扩展浮点数的范围到0到10之间
random_float_0_10 = random_float * 10
print(random_float_0_10)
```

可能的输出为：
```Python
7
0.4356677623
4.356677623
```

## Deep Dive 深入探索
随机数生成从计算机早期就开始了，它依据一定算法产生数字序列，但这些算法是可以预测的，因此被称为“伪随机”。Python通过`random`模块提供易于使用的方法来生成这样的随机数。

`random`模块基于梅森旋转算法，这是一个常见的伪随机数生成器。这个算法在1997年被发明，因为产出高质量的随机数序列而广泛应用于各种编程领域。

与`random`模块相对比的是`numpy`库的随机模块，该库适用于生成大量随机数并且性能较高。还有`secrets`库，它用于生成密码学上安全的随机数。

实现细节上，随机数生成受到初始数（种子）的影响。如果种子相同，生成的随机数序列也相同。故在涉及密码学的场景中，应使用真随机数或安全的伪随机数生成器。

## See Also 另请参阅
- Python `random`模块官方文档：https://docs.python.org/3/library/random.html
- `numpy`的随机数生成模块：https://numpy.org/doc/stable/reference/random/index.html
- `secrets`官方文档（用于生成加密随机数）：https://docs.python.org/3/library/secrets.html