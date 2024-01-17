---
title:                "产生随机数"
html_title:           "Rust: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
随机数生成是指通过计算机程序产生一系列随机的数字或值。程序员经常使用随机数来测试程序的性能，随机生成数据或者为游戏设计产生随机事件。

## 如何：
Rust 中有一个内置的随机数生成器，可以通过引入 crate 来使用。以下是一个简单的例子：

```Rust
use rand::Rng;

fn main(){
    let mut rng = rand::thread_rng(); // 初始化随机数生成器
    let random_num: u32 = rng.gen(); // 生成一个随机的无符号整数
    println!("随机数是： {}", random_num);
}
```

输出：

> 随机数是： 1574519957

## 深入探讨：
随机数生成在计算机科学中有着重要的历史背景，它被广泛地应用于各种领域，如密码学、模拟、和游戏设计。当使用 Rust 的内置随机数生成器时，需要注意随机数的生成需要消耗计算资源，因此在大量使用时需要谨慎。除了使用内置的随机数生成器外，也可以使用第三方 crate，如 `rand` 和 `rand_pcg` 等。

## 另请参阅：
- 关于 Rust 中随机数生成的官方文档：https://doc.rust-lang.org/std/rand/
- 所有 Rust 的第三方随机数 crate：https://crates.io/keywords/random
- 更多关于随机数生成的历史和应用：https://en.wikipedia.org/wiki/Random_number_generation