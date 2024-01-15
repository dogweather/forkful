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

## Why

随机数在编程中经常被用到，它可以帮助我们模拟现实世界和增加程序的多样性。使用随机数也可以帮助我们在处理数据时提高效率和准确性。

## How To

```Python
# 导入random库
import random 

# 生成一个随机整数
num1 = random.randint(1, 10)
# 生成一个随机小数
num2 = random.uniform(1, 10)

# 生成一个随机字符串
string = random.choice("Python Programming")

# 打印随机数
print(num1)
print(num2)
print(string)
```

输出：

5
3.825
h

## Deep Dive

随机数是计算机程序生成的数字，它没有任何明显的模式可循。计算机程序使用伪随机数生成器来产生随机数，在每次运行时都采取不同的种子（seed）来确保生成不同的随机数序列。随机数在模拟游戏、机器学习以及密码学等领域有着重要的作用。

## See Also

- [Python random库文档](https://docs.python.org/3/library/random.html)
- [随机数生成器原理](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [使用随机数的实际应用](https://www.geeksforgeeks.org/applications-of-random-numbers-in-computer-science/)