---
title:                "Elixir: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：

产生随机数在编程中是非常有用的。它可以帮助我们创建各种各样的游戏和应用程序，也可以用于测试和模拟数据。

## 如何操作：

首先，我们需要在代码中导入Elixir的“Random”模块。然后，我们使用`random.uniform/2`函数来生成随机数。以下是一个简单的示例，生成1到10之间的随机数：

```
Elixir

import Random

Random.uniform(1,10)

```

输出可能会是6，3，9等等。

我们也可以使用`random.seed/0`函数来设置随机数的种子值，以确保每次运行程序时都会得到相同的随机数。例如：

```
Elixir

import Random

Random.seed(:elixir)

Random.uniform(1,5)

```

每次运行的输出都会是相同的数字，例如3，4，2。

## 深入了解：

在Elixir中，随机数是根据“伪随机数生成器”算法生成的。它们使用一个种子值来确定生成的随机数序列，因此，如果我们使用相同的种子值，我们将始终得到相同的随机数序列。

同时，我们还可以使用`random.int/2 `函数来生成随机的整数，`random.float/2`函数来生成随机的小数。

另外，我们可以使用`random.uniform/3`函数来生成指定个数的随机数列表，例如：

```
Elixir

import Random

Random.uniform(1,10,5)

```

输出可能会是[3,6,8,4,2]，其中5表示生成5个随机数。

## 请参考：

- Elixir官方文档中关于随机数的介绍: https://elixir-lang.org/getting-started/random-numbers.html
- Elixir的“Random”模块文档: https://hexdocs.pm/elixir/Random.html
- 关于伪随机数生成器的更多信息: https://en.wikipedia.org/wiki/Pseudorandom_number_generator

请参考：

- Elixir官方文档中关于随机数的介绍: https://elixir-lang.org/getting-started/random-numbers.html
- Elixir的“Random”模块文档: https://hexdocs.pm/elixir/Random.html
- 关于伪随机数生成器的更多信息: https://en.wikipedia.org/wiki/Pseudorandom_number_generator