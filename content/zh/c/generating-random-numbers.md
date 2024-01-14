---
title:    "C: 生成随机数"
keywords: ["C"]
---

{{< edit_this_page >}}

## 为什么

随机数在计算机编程中扮演着非常重要的角色。它们可以用来模拟现实世界中的随机事件，也可以用来增加程序的多样性。生成随机数可以让程序更具趣味性，也可以用来加强程序的安全性。因此，学习如何生成随机数在编程中是非常有用的。

## 如何生成随机数

在C语言中，我们可以使用标准函数库中的rand()函数来生成随机数。首先，我们需要在程序开头包含stdlib.h头文件，这样才能使用rand()函数。然后，在main函数中，我们可以使用srand()函数来初始化随机数生成器。最后，我们可以使用rand()函数来生成随机数，并将结果打印出来。

```C
#include <stdlib.h>

int main()
{
    // 初始化随机数生成器
    srand(time(NULL));
    
    // 生成随机数并打印
    int random_num = rand();
    printf("随机数为：%d\n", random_num);
    
    return 0;
}
```

这段代码将会输出类似于以下的结果：

```
随机数为：34567
```

我们也可以通过设定随机数的上限来限制生成的随机数的范围。比如，如果我们只想生成1到10之间的随机数，我们可以使用取余运算符来实现。

```C
int random_num = rand() % 10 + 1;
```

这样生成的随机数就会在1到10之间，包含1和10。

## 深入了解生成随机数

虽然生成随机数可以让我们的程序更加多样化和有趣，但实际上计算机是无法真正生成随机数的。它们只能通过一些特定的算法生成伪随机数。这些算法需要一个种子值来进行计算，并且每次使用相同的种子值生成的随机数序列都是相同的。

这就是为什么我们在使用rand()函数之前需要先调用srand()函数来设定种子值。通常我们可以使用当前的时间作为种子值，这样每次生成的随机数都会不同。但是，如果程序在同一秒钟内执行多次，那么它们将会使用相同的种子值，生成相同的随机数序列。这种情况下，我们可以使用一些其他的种子值来增加随机性，比如进程ID、用户ID等。

## 参考资料

- [如何在C语言中生成随机数](https://www.programiz.com/c-programming/examples/generate-random-numbers)
- [伪随机数生成器](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [随机数的应用](https://www.educba.com/random-number-generator-in-c/)
- [srand()和rand()函数的详细解释](https://www.tutorialspoint.com/c_standard_library/c_function_srand.htm)

## 参见

- [C语言函数库参考](http://www.cplusplus.com/reference/cstdlib/)