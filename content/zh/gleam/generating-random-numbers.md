---
title:    "Gleam: 生成随机数"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 为什么

如果你是一个程序员，那么你可能经常需要使用随机数来解决一些问题。比如，你需要在一个游戏中生成随机地图，或者你需要在一个网络应用程序中为用户生成随机密码。无论什么原因，当你需要随机数时，Gleam可以帮助你轻松地实现这一点。

# 如何

使用Gleam生成随机数非常简单。首先，你需要导入`random`模块：

```Gleam
import random
```

接下来，你可以使用`random.int`函数来生成一个指定范围内的整数：

```Gleam
random.int(1, 10)
```

上面的代码将生成一个介于1和10之间的随机整数。如果你想要生成一个随机的浮点数，你可以使用`random.float`函数：

```Gleam
random.float(0.0, 1.0)
```

你也可以使用`random.bool`函数来生成一个随机的布尔值：

```Gleam
random.bool()
```

最后，如果你想要从一个集合中随机选择一个元素，你可以使用`random.pick_one`函数：

```Gleam
random.pick_one(["a", "b", "c"])
```

每次运行上面的代码，你都会得到一个不同的随机结果。想要了解更多关于随机数的生成，可以查看Gleam文档中关于`random`模块的详细说明。

# 深入探讨

生成随机数看起来很简单，但实际上涉及到一些复杂的算法。在Gleam中，随机数使用的是Mersenne Twister算法，这是一个经过充分测试和改进的高质量随机数生成器。它可以生成几乎无限数量的随机数，每一个都具有高质量的随机性。

此外，Gleam还提供了一些高级的随机数生成函数，如`random.gaussian`来生成服从正态分布的随机数。如果你对随机数背后的原理感兴趣，可以花些时间阅读Gleam的源码，以及相关的数学知识。

# 查看更多

- [Gleam官方文档](https://gleam.run/)
- [Mersenne Twister算法](https://en.wikipedia.org/wiki/Mersenne_Twister)