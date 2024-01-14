---
title:                "Elixir: 生成随机数"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么：为什么要生成随机数

随机数在编程中扮演着重要的角色，它们可以用于模拟现实世界中的随机事件或者用于加密算法。在Elixir编程中，生成随机数可以帮助我们更轻松地处理一些问题，并且让程序更加有趣。

## 如何生成随机数

要在Elixir中生成随机数，我们可以使用内置的Random模块。首先，我们需要导入该模块，然后调用Random.uniform/2函数并传入一个范围值来产生一个随机数。

```elixir
import Random

# 生成一个1到10之间的随机数
Random.uniform(1, 10)
#=> 7
```

我们也可以传入一个浮点数作为范围值来生成随机的小数。

```elixir
# 生成一个0到1之间的随机小数
Random.uniform(0.0, 1.0)
#=> 0.3456
```

除了uniform/2函数，Random模块还提供了其他一些用于生成随机数的函数，如Random.int/1、Random.float/1和Random.seed/1等。

## 深入了解随机数生成

在计算机科学中，真正的随机数是不存在的。所有的随机数都是由一个随机数生成器算法计算出来的。因此，对于同一个种子值，随机数生成器总是会生成相同的随机数序列。在Elixir中，我们可以通过Random.seed/1函数来设置随机数生成器的种子值。

```elixir
# 设置种子值为123，之后对于同一个种子值，都会生成相同的随机数序列
Random.seed(123)
#=> :ok
Random.uniform(1, 10)
#=> 6
Random.uniform(1, 10)
#=> 3
```

另外，Elixir的随机数生成器是基于Mersenne Twister算法实现的，它可以生成高质量的随机数序列。我们还可以使用Random.api_seed/0函数来获取当前的随机数种子值。

## 参考链接

- 关于Elixir中Random模块的官方文档：https://hexdocs.pm/elixir/Random.html
- Mersenne Twister随机数算法的介绍：https://en.wikipedia.org/wiki/Mersenne_Twister

## 参见

- Elixir的官方网站：https://elixir-lang.org/
- Elixir的中文社区网站：http://elixir-cn.com/