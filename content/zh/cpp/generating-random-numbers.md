---
title:                "生成随机数"
html_title:           "C++: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

#什么是随机数生成及其原因？
随机数生成是指在程序中生成无规律的数字或字符串的过程。程序员通常会使用随机数生成来模拟现实世界中的随机性，例如游戏中的掷骰子或发牌等。这可以使程序具有更多的变化性和挑战性。

#如何使用：
```C++
// 创建随机数生成器
srand(time(0));
// 生成0到10之间的随机整数
int randomNum = rand() % 11;
// 生成0.0到1.0之间的随机小数
double randomDecimal = (double)rand() / RAND_MAX;
```

输出：
随机整数：5
随机小数：0.837636

#深入探讨：
随机数生成的历史可以追溯到20世纪50年代，最早用于模拟核武器测试计算。目前，生成随机数的方法有多种，包括伪随机数生成器和真随机数生成器。伪随机数生成器利用数学算法生成序列，而真随机数生成器则利用物理过程或外部设备来产生随机数。在编程中，通常使用伪随机数生成器来满足需求。

#相关信息：
更多关于随机数生成的内容，可以参考下面的链接：
- [C++生成随机数的方法](https://www.cplusplus.com/reference/cstdlib/rand/)
- [随机数生成器的实现原理](https://en.wikipedia.org/wiki/Random_number_generation)