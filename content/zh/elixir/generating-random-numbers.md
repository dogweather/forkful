---
title:                "生成随机数"
html_title:           "Elixir: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么: 即使我们是程序员，但有时候我们也需要一些随机性。生成随机数字可以用于测试，模拟和随机选择，这让我们的程序更加有趣。

## 为什么

每个程序员都知道，编写可靠的代码是至关重要的。而测试是确保我们的代码按预期工作的一种方法。但是，测试很容易陷入固定模式，可能无法覆盖所有情况。这时候，我们可以使用随机生成的数字来测试我们的程序，以产生更多不同的情况，从而提高代码的质量。

## 如何

使用 Elixir 中的 `:rand` 模块，我们可以轻松地生成随机数字。例如，让我们生成一个随机的 10 位数:

```elixir
:rand.uniform(1000000000)
```

该代码将产生类似于 `524645302` 的输出。如果我们想生成一个介于 1 到 100 之间的随机数，可以使用 `:rand.uniform(1..100)`。Elixir 还提供了其他一些函数来生成不同类型的随机数，例如 `:rand.uniform/2` 可以生成浮点数，`rand.uniform/3` 可以生成多个随机数。具体可参考[官方文档](https://hexdocs.pm/elixir/Random.html)。

## 深入探讨

在 Elixir 中，随机数生成是基于一个种子(seed)。一个种子决定了随机数的序列，所以如果我们使用相同的种子，每次生成的随机数都是一样的。我们可以使用 `:rand.seed/1` 来设置种子，也可以在需要的地方使用 `:rand.seed/0` 来获取当前的种子。

另外，有时候我们可能想要生成不完全随机的数字，例如通过加密算法，Elixir 也提供了 `:rand.uniform/1` 和 `:rand.uniform!/1` 来实现这一点。

## 参考阅读

- [Elixir 随机数生成文档](https://hexdocs.pm/elixir/Random.html)
- [关于随机数生成的一篇博客文章](https://elixir-lang.org/getting-started/randomness.html#generating-random-numbers)