---
title:    "Elixir: 生成随机数"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：如果你正在寻找一种简单的方法来生成随机数，Elixir的内置功能可以帮助你达到这一目的。无论是为了生成测试数据或者游戏，生成随机数都是很有用的。

怎么做：使用Elixir自带的 ```rand/0``` 和 ```rand/1``` 函数来生成随机数。例如：

```elixir
rand()
# => 0.7347234097808059

rand(1..10)
# => 7
```

深入探讨：Elixir使用Mersenne Twister算法来生成随机数，这是一种高效并且产生高质量随机数的算法。它可以生成非常大的随机数（2^19937-1），同时保持很低的内存占用。

另外，Elixir还提供了一个有用的函数 ```shuffle/1``` 来对一个列表进行随机化操作。这在编写简单的游戏或者打乱数据顺序时非常有用。

其他的一些有用的随机数生成函数还包括 ```uniform/1``` （生成0到1之间的随机小数）和 ```uniform!/1``` （生成指定范围之内的随机小数）。

最后，使用 ```Seed.random/0``` 可以获取一个当前时刻的随机种子，这个种子可以在需要可复现的随机数的情况下使用。

## 参见
- [Elixir官方文档 - 随机数生成器](https://hexdocs.pm/elixir/Kernel.html#rand/1)
- [Elixir官方文档 - List模块](https://hexdocs.pm/elixir/List.html#shuffle/1)
- [Elixir官方文档 - 内置Seed模块](https://hexdocs.pm/elixir/Seed.html)