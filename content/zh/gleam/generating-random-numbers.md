---
title:                "生成随机数"
date:                  2024-01-27T20:33:34.519055-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？

在编程中生成随机数对创建模拟、测试、密码学和游戏至关重要。在 Gleam 中，这是一个功能，允许开发者将不可预测性引入到他们的应用中或模拟现实世界的场景。

## 如何操作：

要在 Gleam 中生成随机数，主要使用 `gleam_random` 库。这个库提供了生成随机整数、浮点数等的函数。首先，确保你已经将 `gleam_random` 添加到你的 `rebar.config` 或 `mix.exs` 文件中作为依赖。

让我们深入一些例子：

### 生成一个随机整数

要生成一个指定范围内的随机整数，你可以使用 `int` 函数：

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

此函数将生成一个 1 到 10 之间（含）的随机整数。

### 生成一个随机浮点数

要获得一个随机浮点数，请使用 `float` 函数。这将生成一个在 0.0 到 1.0 之间的浮点数：

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### 示例输出

运行这些函数可能会产生像这样的输出：

- 对于 `generate_random_int()`：`5`
- 对于 `generate_random_float()`：`0.84372`

记住，由于随机性的本质，每次执行可能会导致不同的输出。

## 深度探讨

`gleam_random` 模块实现了一个伪随机数生成器 (PRNG)，这本质上意味着这些数字不是真正的随机，但是很难预测，模仿了随机性。PRNG 通过从一个初始值开始，即种子，并应用数学运算来生成一个数列。

从历史上看，语言和库已经实现了几种 PRNG 算法，如 Mersenne Twister 或线性同余生成器 (LCG)。算法的选择影响到 "随机性" 的质量，有些算法更适合用于密码学应用。虽然 Gleam 的标准库通过其 `gleam_random` 模块提供了便利和易用性，但对于需要密码学安全随机性的用例，它可能并不总是最好的选择。对于密码学目的，开发者应该考虑专门设计用于提供密码学安全伪随机数生成器 (CSPRNGs) 的库，这些库被设计用来抵御那些基于观察生成的数列来预测未来数的攻击。

总之，尽管 Gleam 的随机数生成功能对于一般编程需求是健壮的，但具有特定安全要求的应用程序应当考虑使用专门的密码学解决方案，以确保其随机数生成的完整性和安全性。
