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

## 为什么

随机数生成是编程中常见的任务，可以让程序具备随机性和变化性，从而提高用户体验和程序的可玩性。它在游戏、模拟、加密等领域都有广泛的应用，并且可以帮助解决许多实际问题，比如生成随机密码和创建测试数据等。

## 如何实现

要在C++中生成随机数，我们可以使用标准库中的rand()函数。它会返回一个介于0到RAND_MAX之间的伪随机整数，需要结合srand()函数和time库来设置随机数种子，使得每次运行程序时得到的随机数序列都不同。

```C++
#include <cstdlib>
#include <ctime>
#include <iostream>

int main() {
    // 设置随机数种子
    srand(time(nullptr));
    // 生成0到100之间的随机数
    int num = rand() % 101;
    // 输出结果
    std::cout << "随机数为：" << num << std::endl;
    return 0;
}
```

运行结果可能为：

```
随机数为：49
```

除了使用rand()函数，我们还可以使用C++11中引入的random库来生成更为高质量的随机数。它提供了多种随机数引擎和分布函数，可以根据不同情况选择最合适的方式生成随机数。

```C++
#include <random>
#include <iostream>

int main() {
    // 创建随机数引擎
    std::random_device rd;
    std::mt19937 gen(rd());
    // 创建分布函数，并设置范围为1到100
    std::uniform_int_distribution<> dist(1, 100);
    // 生成随机数
    int num = dist(gen);
    // 输出结果
    std::cout << "随机数为：" << num << std::endl;
    return 0;
}
```

运行结果可能为：

```
随机数为：75
```

## 深入了解

随机数生成的核心其实是一个数列，这个数列中的数字按照一定的规律来获取，但又看起来像是无规律的。比如，rand()函数使用的是线性同余法来生成伪随机数，而random库则使用更复杂的算法。为了保证随机数的质量，我们需要避免一些常见的陷阱，如在短时间内多次调用rand()函数和未正确设置随机数种子等。

## 参考文献

- [C++ reference](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ random numbers](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Random Numbers: Useful tips and common pitfalls](https://www.learncpp.com/cpp-tutorial/random-number-generation/)
- [An Introduction to Random Number Generation](https://arxiv.org/pdf/1704.05312.pdf)

## 参见

- [C++入门教程](https://github.com/bodged/beginning-cpp-zh)
- [C++编程规范](https://github.com/SuperV1234/cpp-style-guide/blob/master/README.zh-CN.md)