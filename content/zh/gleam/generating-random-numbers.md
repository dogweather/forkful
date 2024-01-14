---
title:                "Gleam: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

为什么要生成随机数？在编程中，随机数是非常有用的。它可以帮助我们模拟真实世界的情况，并且常用于游戏开发、密码学和实验设计等领域。

## 如何做

使用Gleam编程语言可以轻松地生成随机数。下面是一个简单的示例：

```Gleam
import random

random.generate_int(1, 10)
```

输出可能是6或者3，因为它会在给定的范围内生成一个随机整数。你也可以指定生成浮点数、列表、字符串等不同类型的随机数。

```Gleam
random.generate_float(0.0, 1.0)
random.generate_list([1,2,3,4,5,6], 3)
random.generate_string(10)
```

## 深入了解

Gleam中的随机数函数是基于Mersenne Twister算法实现的，这是一种高质量的随机数生成器。它的实现方式非常高效，可以满足大多数编程需求。

如果你希望了解更多关于随机数生成的原理和算法，可以阅读文档中的《Random模块》部分。

## 参考资料

- [文档：Random模块](https://gleam.run/documentation/0.13.2/random/)
- [Mersenne Twister算法介绍](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [如何在Gleam中使用随机数](https://github.com/gleam-lang/gleam/discussions/286)