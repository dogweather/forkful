---
title:                "生成随机数"
date:                  2024-01-20T17:49:24.871898-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
生成随机数就是创建没有明显模式的数字序列。程序员用它们来做游戏、模拟、安全系统和测试。

## How to: 怎么做？
在 Gleam 中，你可以使用 `gleam_stdlib` 包来生成随机数：

```gleam
import gleam_stdlib/random as random

pub fn roll_die() -> Int {
  random.int(1, 6)
}
```

样例输出可能是 `4`。记得每次运行程序，结果可能都不一样。

## Deep Dive 深入了解
随机数在计算机中是个老话题了。我们不能生成真正的随机数，只能生成伪随机数。伪随机数算法的选择可以影响到数字的分布。

其他语言可能有不同的随机数生成库或函数。比如说，Python 有 `random`、JavaScript 有 `Math.random()`。

Gleam 在底层是使用 Erlang 的随机数生成器。对于大多数用途来说，这些伪随机数已经足够好了。不过，如果要求很高的加密安全系统，则需要特别设计的硬件或算法。

## See Also 参考链接
- [Gleam stdlib documentation](https://hexdocs.pm/gleam_stdlib/)
- [Erlang random module](http://erlang.org/doc/man/rand.html)
- [Wikipedia on Pseudorandomness](https://en.wikipedia.org/wiki/Pseudorandomness)