---
title:    "C: 生成随机数"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么会需要生成随机数

在计算机编程中，我们经常需要使用随机数来模拟真实世界的情况，或者为我们的程序增加一些变数和随机性。例如，游戏开发中经常使用随机数来生成不同的游戏场景，增加游戏的可玩性。因此，生成随机数是一种非常重要的编程技巧。

## 如何生成随机数

在 C 语言中，我们可以使用标准库函数 `rand()` 来生成随机数。首先，我们需要包含标准库头文件 `stdlib.h`，然后使用 `srand()` 函数来设置随机数的种子。在接下来的代码中，我们可以使用 `rand()` 函数来生成一个范围在 0 到 99 之间的随机数，并将其输出到屏幕上。

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // 设置随机数种子
    srand(1234);
    // 生成随机数并输出
    int num = rand() % 100;
    printf("随机数为：%d", num);
    return 0;
}
```

输出结果：
```
随机数为：47
```

## 深入了解生成随机数

虽然我们可以通过上面的方法很容易地生成随机数，但实际上，程序生成的随机数并不是真正的随机数，而是伪随机数。这是因为计算机程序只能按照特定的算法来生成随机数，因此我们可以通过设置相同的随机数种子来得到相同的随机数序列。

为了让程序生成更加真实的随机数，我们可以利用当前的时间来设置随机数种子，这样每次运行程序，都会得到不同的随机数序列。另外，我们也可以使用更复杂的随机数生成算法来提高随机数的质量。

## 参考阅读

- [C 语言标准库函数 - rand()](https://baike.baidu.com/item/rand/3533587?fr=aladdin)
- [随机数生成算法](https://baike.baidu.com/item/%E9%9A%8F%E6%9C%BA%E6%95%B0%E7%94%9F%E6%88%90%E7%AE%97%E6%B3%95/5252485?fr=aladdin)
- [随机数 - 维基百科](https://zh.wikipedia.org/wiki/%E9%9A%8F%E6%9C%BA%E6%95%B0)

# 参见

- [C语言随机数生成方法（Windows环境）](https://blog.csdn.net/leonard_kean/article/details/25675779)
- [Linux C 程序设计（第二版） · 第 3 章 C 库函数 ∙ 3.4 伪随机数生成](https://www.shiyanlou.com/courses/54)