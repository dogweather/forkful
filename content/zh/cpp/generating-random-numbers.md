---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
生成随机数是指计算机程序制作出一串看起来随机的数字。程序员之所以这么做，是因为随机数在许多地方都很有用，比如在模拟，游戏，随机抽选和加密等方面。

## 如何:
我们可以用C++的 `<random>`库来生成随机数。以下是一个简单的示例:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;  
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<> distr(1, 10); 

    for(int n=0; n<16; ++n)
        std::cout << distr(gen) << ' '; 
    return 0;
}
```
这会创建一个输出16个1到10之间的随机数字的程序。

## 深入探讨
随机数生成在计算机的早期历史中就已经存在了，早在第一台真正赋予计算功能的机器ENIAC中即已实现。与 `<random>`库不同，有一些替代方法，例如 `rand()`，但这个函数在产生绝对随机数上并不是很完美。这是因为 `rand()`返回的是伪随机数，这些数字在有限的范围内显示出随机性，但随着时间的推移，它们将开始显示出模式。这对于许多应用（尤其是安全相关的应用，如加密）来说，可能是个问题。

C++的 `<random>`库的出现完全改变了这个局面。该库在C++11中被引入，并提供了多种生成高质量随机数的设备以及多种统计分布选项。

## 另请参阅
- [C++ `rand()`函数](http://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ `<random>`库教程](https://en.cppreference.com/w/cpp/numeric/random)
- [早期计算机中的随机数生成](https://en.wikipedia.org/wiki/Random_number_generation#Early_computers_and_games)