---
title:    "C++: 生成随机数"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 为什么使用随机数生成器

随机数生成器在编程中是一个非常常见的工具。它允许我们生成随机数，这在很多应用中都是必不可少的。比如游戏设计、随机密码生成等等。通过使用随机数，我们可以增加程序的变化性和挑战性。它也能够使得我们的程序更加真实和多样化。因此，在进行编程时，学习如何使用随机数生成器是非常有用的。

# 如何使用随机数生成器

使用C++语言编写一个简单的随机数生成器非常简单。首先，我们需要包含C++中的random库。

```C++
#include <random>
```

然后，我们可以使用random库中的不同函数来生成不同类型的随机数。比如，我们可以使用uniform_int_distribution函数来生成一个指定范围内的整数随机数。

```C++
#include <iostream>
#include <random>

int main() {
    //生成一个1到100的随机整数
    std::random_device rd;   //用于随机数种子
    std::mt19937 gen(rd());  //用于调用随机数引擎
    std::uniform_int_distribution<int> dis(1, 100);  //指定生成随机数范围
    int random_num = dis(gen);  //调用随机数引擎生成随机数
    std::cout << "生成的随机数是：" << random_num;
    
    return 0;
}
```

运行结果可能是：

```
生成的随机数是：56
```

除了生成整数随机数，我们也可以使用uniform_real_distribution函数来生成指定范围内的小数随机数。同时，如果我们想生成不同类型的随机数，比如布尔值、字符等，也有相应的函数可以使用。

# 深入了解随机数生成器

在背后，随机数生成器实际上使用了一个伪随机数算法。这个算法根据一个初始的随机种子，使用一系列复杂的计算来生成一系列随机数。因此，同样的初始种子会生成同样的随机数序列。这也是为什么我们在每次使用随机数的时候都需要使用一个随机设备来作为种子，以保证每次生成的随机数都是不同的。

另外，随机数生成器也有一些参数可以调整，比如均匀性、分布形状等。这一般会影响到生成的随机数的分布情况。如果我们想要调整随机数的分布情况，可以使用对应的参数来实现。

# 参考链接

- [C++标准库中的随机数生成器](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [C++ random库文档](https://en.cppreference.com/w/cpp/numeric/random)
- [伪随机数生成器](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)

# 参见

- [随机数生成器的实际应用](https://blog.csdn.net/xingyeyiyi/article/details/77971265)
- [使用C++生成随机密码的方法](https://www.cnblogs.com/haozhixiong7/p/13175111.html)