---
title:    "C++: 生成随机数"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，生成随机数是一个非常常见的需求。无论是用于游戏开发、密码学还是其他领域，生成随机数都是必不可少的。它可以给程序带来一些不确定性，从而让它们变得更加灵活和有效。

## 如何生成随机数
生成随机数的方法有很多种，但在C++中，我们通常会使用标准库中的rand()函数来实现。首先，我们需要包含<cstdlib>头文件，并使用srand()函数来设置随机数种子。接着就可以使用rand()函数来生成一个随机数了。下面是一个示例代码和输出：

```C++
#include <cstdlib>
#include <iostream>

int main() {
  // 设置随机数种子
  srand(time(NULL));
  
  // 生成一个0-10之间的随机数
  int random_num = rand() % 11;
  
  // 输出结果
  std::cout << "随机数： " << random_num;
  
  return 0;
}
```

输出结果可能类似于：随机数： 4。我们可以使用取模运算来限制随机数的范围，在本例中就是0-10。

## 深入了解生成随机数
在C++中，rand()函数实际上是伪随机数生成器，它会根据种子值来生成一个序列。这意味着，如果我们使用相同的种子值，就会得到相同的随机数序列。因此，为了让我们的程序每次运行都生成不同的随机数，我们需要使用一个随机的种子值，比如当前的时间。这就是为什么我们在前面的示例代码中使用了time(NULL)来设置随机数种子。

除了使用rand()函数之外，C++11标准库中还提供了更加强大的随机数生成器功能，比如uniform_int_distribution和mt19937。这些函数和类可以让我们更加灵活地生成随机数。有关这方面更多的内容，可以参考下面的参考链接。

## 参考链接
- [C++标准库中的rand()函数文档](http://www.cplusplus.com/reference/cstdlib/rand/)
- [C++11引入的随机数库文档](http://www.cplusplus.com/reference/random/)
- [mt19937随机数引擎文档](http://www.cplusplus.com/reference/random/mt19937/)
- [如何正确使用随机数生成器](https://en.cppreference.com/w/cpp/numeric/random)