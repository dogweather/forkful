---
title:                "Python: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：为什么会有人想要生成随机数？

他们可能想要创建一个随机化的程序，或者在游戏中使用随机数来增加游戏性。随机数也可以用于模拟实验，以及在数据分析中产生随机样本。

如何：使用Python生成随机数的方法如下： 

```Python 
# 导入random模块
import random 

# 生成一个0-1之间的随机数
random_number = random.random()
print(random_number)

# 生成一个范围内的随机整数
random_int = random.randint(1, 10)
print(random_int)

# 生成一个指定范围和步长的随机浮点数
random_float = random.uniform(1.5, 3.5, 0.5)
print(random_float)

# 从一个序列中随机选择一个元素
my_list = [1, 2, 3, 4, 5]
random_choice = random.choice(my_list)
print(random_choice) 

# 打乱一个序列中的元素顺序
random.shuffle(my_list)
print(my_list)
```

输出结果：

0.524632782334
7
2.789
3

深入了解：生成随机数不是一个简单的过程，它涉及到复杂的数学算法。Python中的random模块使用的是Mersenne Twister算法，它是一种伪随机数生成器。这意味着生成的随机数看起来是随机的，但实际上是根据一个固定的种子开始的。如果使用相同的种子，将会生成相同的随机数序列。

此外，生成的随机数也受到计算机系统的影响。因为计算机是基于算法和固定的输入，所以它不能产生真正意义上的随机数。因此，在使用随机数时，应该保持谨慎，并避免依赖它们来保证程序的安全性。

另一个值得注意的问题是随机数的种子。一些应用程序可能需要固定的种子来保证生成一致的随机数序列，而其他应用程序可能需要更多的随机性，需要使用不同的种子来生成不同的随机数序列。

参考链接：

- [Python官方文档 - 随机数生成器](https://docs.python.org/2/library/random.html)
- [深入理解随机数生成算法](https://cs.wikipedia.org/wiki/Mersenne_Twister)
- [如何使用随机数生成器](https://www.digitalocean.com/community/tutorials/how-to-use-the-random-number-generator-in-python-3)

同样了解： 

- [如何使用Python创建一个随机数密码生成器] (https://www.askpython.com/python-modules/python-password-generator)
- [如何使用随机数来改变程序的运行方式](https://code.tutsplus.com/tutorials/random-number-generation-in-python--cms-26563)
- [如何使用随机数优化图像处理过程](https://medium.com/quick-code/optimize-image-processing-performance-with-this-simple-python-technique-8112e406c355)

请参阅：

- 更多关于Python的随机数生成器文章 [https://www.askpython.com/](https://www.askpython.com/)
- Python的官方文档 [https://docs.python.org/2/](https://docs.python.org/2/)