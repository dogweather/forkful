---
title:                "产生随机数"
html_title:           "C: 产生随机数"
simple_title:         "产生随机数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么是生成随机数?为什么程序员要这么做?

生成随机数是一种计算机编程技术，它允许程序员生成一个随机的数字，而不是固定的值。程序员经常使用这种技术来增加程序的复杂性和随机性，使其更具有可玩性和挑战性。

## 如何实现:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void) {
  
  int random_num1 = 0;
  int random_num2 = 0;
  srand(time(0)); //设置seed

  //生成一个1到10之间的随机数
  random_num1 = (rand() % 10) + 1;

  //生成一个0到100之间的随机数
  random_num2 = rand() % 101;

  printf("第一个随机数: %d\n", random_num1);
  printf("第二个随机数: %d\n", random_num2);

  return 0;
}

```

输出:

```
第一个随机数: 6
第二个随机数: 42
```

## 深入探讨:

生成随机数的历史可以追溯到20世纪早期的数学研究。在计算机科学中，有几种方法来实现生成随机数，包括伪随机数生成器，硬件随机数生成器和完全随机数生成器。程序员可以根据具体的需求来选择最适合的方法。

## 查看更多:

- [《C语言程序设计》- 第九章：随机数](https://book.douban.com/subject/30280646/)
- [C语言官方文档-stdlib.h](https://zh.cppreference.com/w/c/numeric/random)
- [透过随机数生成器巧思无穷](https://juejin.im/post/5c9ca188f265da60e37294f4)