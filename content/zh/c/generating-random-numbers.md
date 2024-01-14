---
title:                "C: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

在计算机编程中，生成随机数是一个非常有用的技巧。它可以帮助我们解决一些难题，也可以增加程序的趣味性。无论是在游戏开发、随机算法还是密码学中，随机数都扮演着重要的角色。

## 如何

如果你想在C语言中生成随机数，你可以使用`rand()`函数。首先，需要包含`stdlib.h`头文件。然后，在你需要生成随机数的位置，使用`rand()`函数，并将其赋值给一个变量。例如：

```C
#include <stdlib.h>

int main() {
    int random_number = rand(); // random_number将会是一个随机数
}
```

不过，单单使用`rand()`函数并不能保证每次都生成不同的随机数。为了达到更好的随机性，我们可以先使用`srand()`函数播种随机数发生器，再使用`rand()`函数生成随机数。我们可以使用当前时间作为播种的种子，确保每次生成的随机数都不同。例如：

```C
#include <stdlib.h>
#include <time.h>

int main() {
    // 使用当前时间作为种子，种子只需要设置一次
    srand(time(0));

    int random_number = rand(); // random_number将会是一个随机数
}
```

通过使用`srand()`函数，我们可以在程序每次运行时都得到不同的随机数。除此之外，我们还可以使用`rand()`函数的参数来控制随机数的范围。例如，要生成0-9的随机数，可以使用`rand() % 10`，这样得到的结果永远不会超过10。还可以结合`srand()`函数一起使用，如下所示：

```C
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0));

    // 生成0-9的随机数
    int random_number = rand() % 10;
}
```

## 深入探究

我们可以看到，生成随机数并不是一件非常复杂的事情。但是，要达到真正的随机性，我们需要更复杂的算法。在计算机科学中，真正的随机数是不存在的，因为计算机都是基于一系列固定的指令来工作的。因此，我们需要使用伪随机数生成算法，来模拟真正的随机性。

伪随机数生成算法需要一系列的参数来产生随机数，我们可以称之为“种子”。种子可以是任何非负整数，但是为了实现更好的随机性，我们往往会使用当前时间、程序运行时间、鼠标的位置等作为种子。

在计算机科学中，有几种流行的伪随机数生成算法，比如线性同余法、梅森旋转算法等。这些算法主要通过一系列迭代运算，来生成伪随机数。

## 参考链接

- [C语言随机数生成示例](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [伪随机数生成算法](https://zh.wikipedia.org/zh-hans/%E4%BC%AA%E9%9A%8F%E6%9C%BA%E6%95%B0%E7%94%9F%E6%88%90)
- [随机性和伪随机数算法](https://blog.csdn.net/gc817724278/article/details/5900837)

## 另请参阅

- [C语言学习指南](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C语言随机数生成函数查看手册](https://www.cplusplus.com/reference/c