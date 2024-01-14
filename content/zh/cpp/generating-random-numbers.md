---
title:                "C++: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

"为什么"本文将介绍如何在C++中生成随机数字，并通过代码示例和深入探讨解释这一过程的原理和意义。 C++是一种流行的编程语言，经常被用于编写大型和复杂的程序，而随机数字生成是一个广泛应用的技术。

## 为什么

生成随机数字在编程中是一个常见的需求。它可以被用来创建各种功能，例如游戏中的随机事件、数据加密、模拟实验、密码生成等。随机数字可以让程序变得更加有趣和多样化，也可以提高程序的安全性。

## 如何实现

在C++中生成随机数字需要使用标准库中的随机数生成器（rand()）函数。该函数需要一个种子作为输入，种子可以是任意的整数值，通常使用系统时间作为种子，这样每次程序运行时都会生成不同的随机数字序列。下面是一个简单的示例代码：

```C++
#include <iostream>
#include <ctime>
#include <cstdlib>

using namespace std;

int main() {
    // 设置种子为系统时间
    srand(time(0));

    // 生成5个随机数
    for (int i = 0; i < 5; i++) {
        // 使用取模运算来限制随机数的范围
        int num = rand() % 1000;
        // 输出随机数
        cout << num << " ";
    }

    return 0;
}
```

输出结果可能为：

```
845 3 267 517 942
```

## 深入探讨

随机数字生成是一个涉及到概率论的复杂问题。在计算机中，所有的随机数字实际上都是通过一系列确定性的计算得到的。因此，我们所说的“随机”实际上是一种伪随机的概念。C++标准库中的随机数生成器使用了线性同余法来生成伪随机数字。该方法使用一个乘数、一个加数和一个模数来计算下一个随机数，其中模数控制了随机数的范围。由于种子的不同，最终生成的随机数序列也会有所不同。

## 参考资料

- [cplusplus.com - rand()](http://www.cplusplus.com/reference/cstdlib/rand/)
- [GeeksforGeeks - Generating Random Numbers in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Wikipedia - Pseudorandom Number Generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)

## 参见

- [精通C++随机数生成器](https://www.zhihu.com/question/265768392)
- [如何在C++中生成随机数](https://www.jianshu.com/p/8b7f8af82ea6)
- [随机数生成器的原理与实现](https://www.jianshu.com/p/809a67025b20)