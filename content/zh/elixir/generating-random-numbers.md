---
title:                "产生随机数"
html_title:           "Elixir: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么是随机数生成？ 为什么程序员要做随机数生成？

随机数生成是指在程序中产生随机数的过程。程序员经常需要使用随机数来模拟真实世界中的随机事件，例如游戏中的骰子投掷或抽奖活动。它也可以用于密码学中生成安全的随机数，以确保数据的保密性和完整性。

## 如何做？

以下是使用 Elixir 语言生成随机数的代码示例：

```Elixir
# 生成一个介于0和100之间的随机整数
rand_num = Enum.random(0..100)
IO.puts(rand_num)

# 生成一个包含5个随机元素的列表（使用 Kernel模块中定义的 :rand.uniform/0 方法）
rand_list = List.new(fn -> :rand.uniform() end, 5)
IO.inspect(rand_list)
```

运行以上代码，将会输出类似以下结果：

```Elixir
47
[0.18369, 0.82039997, 0.473719, 0.022033, 0.911895]
```

## 深入了解

随机数生成已经存在很久了，并且有许多不同的实现方法。在计算机科学领域，有一种称为“伪随机数生成器”的算法，它们可以按照一定的规则生成看起来随机的数。然而，这些数并不是真正的随机数，因为它们的生成过程是可预测的。

除了 Elixir 内置的随机数生成方法外，还有一些第三方库可以实现不同种类的随机数生成，例如 [`:rand32`](https://hex.pm/packages/rand32) 和 [`:murmur`](https://hex.pm/packages/murmur)。

在 Elixir 中，随机数生成依赖于 [`:rand`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/0) 和 [`:math`](https://hexdocs.pm/elixir/Math.html#functions) 模块，它们提供了各种用于生成随机数的方法，包括生成整数、浮点数、布尔值和字符。

## 参考链接

- [Elixir 官方文档中关于随机数的介绍](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/0)
- [其他随机数生成库的介绍](https://hexdocs.pm/elixir/random-libraries.html)