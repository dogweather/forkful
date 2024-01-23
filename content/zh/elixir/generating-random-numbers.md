---
title:                "生成随机数"
date:                  2024-01-20T17:49:04.572905-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

生成随机数指的是产生没有固定模式，不可预测的数字序列。程序员生成随机数来进行测试、模拟或为应用程序增加不确定性和多样性。

## How to: (如何操作：)

在Elixir中生成随机数很直接。我们将使用`rand/0`函数来生成一个随机数，然后使用`rand/1`函数带参数来限制随机数的范围。

```elixir
# 生成随机浮点数
random_float = :rand.uniform()
IO.puts(random_float)

# 生成一个1到10之间的随机整数
random_integer = :rand.uniform(10)
IO.puts(random_integer)
```

你会得到类似下面的输出，但每次输出都会改变，因为...嗯，它是随机的嘛！

```
0.44358461783080203
7
```

## Deep Dive (深潜):

在历史上，Elixir依赖于Erlang提供的`rand`模块来生成随机数。事实上，Elixir中大多数的随机数功能都是直接引用自Erlang。一些替代方案，如伪随机数生成器(PRNGs)在不同的编程语言中有不同实现，但核心概念是相同的：尽可能地模拟真正的随机性。在Elixir中使用`:rand.uniform()`函数时，你其实使用的是一个经过精心设计的算法，这个算法可以生成均匀分布的伪随机数。

一个重要的实现细节是，`:rand.uniform()`在第一次调用时会自动为其随机数发生器种子初始化。这意味着无需手动介入，但如果你需要可重现的结果集，也可以手动设置种子。

## See Also (另请参阅):

- Elixir官方文档关于`rand`模块: [https://hexdocs.pm/elixir/1.12/Kernel.html#rand/0](https://hexdocs.pm/elixir/1.12/Kernel.html#rand/0)
- Erlang官方文档关于`:rand`模块: [http://erlang.org/doc/man/rand.html](http://erlang.org/doc/man/rand.html)
- Wikipedia上关于伪随机数生成器的更多信息: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
