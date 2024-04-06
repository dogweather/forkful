---
date: 2024-01-27 20:33:18.564542-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Elixir \u4E2D\u751F\u6210\
  \u968F\u673A\u6570\uFF0C\u4F60\u4E3B\u8981\u4F7F\u7528 `:rand` \u6A21\u5757\uFF0C\
  \u5B83\u63D0\u4F9B\u4E86\u6B64\u76EE\u7684\u7684\u51E0\u4E2A\u51FD\u6570\u3002\u8FD9\
  \u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u6307\u5357\uFF0C\u53EF\u4EE5\u5E2E\u52A9\u4F60\
  \u5F00\u59CB\uFF1A \u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u4E3A\u968F\u673A\u6570\u751F\
  \u6210\u5668\u63D0\u4F9B\u79CD\u5B50\uFF0C\u4EE5\u7528\u552F\u4E00\u7684\u8D77\u70B9\
  \u521D\u59CB\u5316\u5B83\uFF1A."
lastmod: '2024-04-05T22:38:46.529107-06:00'
model: gpt-4-0125-preview
summary: "rand` \u6A21\u5757\uFF0C\u5B83\u63D0\u4F9B\u4E86\u6B64\u76EE\u7684\u7684\
  \u51E0\u4E2A\u51FD\u6570\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u6307\u5357\
  \uFF0C\u53EF\u4EE5\u5E2E\u52A9\u4F60\u5F00\u59CB\uFF1A \u9996\u5148\uFF0C\u786E\u4FDD\
  \u4F60\u4E3A\u968F\u673A\u6570\u751F\u6210\u5668\u63D0\u4F9B\u79CD\u5B50\uFF0C\u4EE5\
  \u7528\u552F\u4E00\u7684\u8D77\u70B9\u521D\u59CB\u5316\u5B83\uFF1A."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
要在 Elixir 中生成随机数，你主要使用 `:rand` 模块，它提供了此目的的几个函数。这里是一个快速指南，可以帮助你开始：

首先，确保你为随机数生成器提供种子，以用唯一的起点初始化它：

```elixir
:rand.seed(:exsplus)
```

要生成一个范围内的随机整数，请使用：

```elixir
random_integer = :rand.uniform(10) # 生成一个1到10之间的数字
IO.puts(random_integer)
```

生成一个0到1.0之间的随机浮点数：

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

你可能需要一个更具体范围的浮点数，这需要稍微多做一些计算：

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

记住，这些数字是伪随机的；它们由种子和算法决定，但对于大多数应用来说足够用了。

## 深入探讨
Elixir 的随机数生成能力依赖于 Erlang 的 `:rand` 模块，反映了它的血统和与 Erlang 的密切关系。`:rand` 模块替换了较旧的 `:random` 模块，提供了改进的随机数生成算法。它提供了多种算法，缺省值为 `exsplus`，但也支持其他算法，如 `exs64`、`exsl` 等，每个算法在速度和随机性质量方面都有其权衡。

Elixir（因此也是Erlang）随机数生成的一个有趣方面是它对种子的处理。系统为每个进程维护单独的种子状态，确保并发进程不会干扰彼此的随机数字序列。这对于并行应用程序特别有用，确保分布式系统中的可预测性和可靠性。

虽然 `:rand` 模块对于大多数用例来说已经足够，但需要加密安全的随机数的应用应考虑其他选项。`crypto` 模块提供的函数如 `crypto:strong_rand_bytes/1` 被设计用来生成适合加密目的的安全随机数据。这些替代选项对于安全敏感的应用来说至关重要，如令牌生成、加密和某些类型的认证机制。
