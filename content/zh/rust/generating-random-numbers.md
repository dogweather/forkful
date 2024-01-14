---
title:                "Rust: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数？

生成随机数在编程中是一个非常常见的需求。它可以用来模拟真实世界的情况，比如抛硬币或者掷骰子的结果。此外，在加密应用程序中，生成高质量的随机数也是非常重要的。无论你的需求是什么，学习如何在Rust中生成随机数都是值得的。

## 如何生成随机数

Rust中有一个内置的rand库可用于生成随机数。首先，我们需要在程序的 `#[crate]` 部分引入这个库：

```Rust
extern crate rand;
use rand::Rng;
```

接下来，我们可以使用 `rand::thread_rng()` 和 `gen_range()` 函数来生成一个0-10之间的随机整数：

```Rust
let random_number = rand::thread_rng().gen_range(0, 10);
println!("随机数: {}", random_number);
```

运行上面的代码，每次都会得到不同的随机数，比如 `3`、`7` 或者 `9`。

## 深入探讨随机数生成

生成高质量的随机数并不是一件简单的事情。在Rust中，使用 `thread_rng()` 函数生成的随机数是伪随机数，也就是说它们并非真正的随机数。为了生成更好的随机数，我们可以使用 `OsRng` 随机数生成器，它依赖操作系统提供的随机数源。

除了生成整数，rand库还提供了其他类型的随机数生成器，如 `gen_range()` 也支持生成浮点数：

```Rust
let random_float = rand::thread_rng().gen_range(0.0, 1.0);
println!("随机浮点数: {}", random_float);
```

此外，rand库还提供了种子生成器，用于生成可复现的随机数序列。这对于调试和测试非常有用。

## 参考链接

- [Rust官方文档 - rand库介绍](https://doc.rust-lang.org/std/rand/)
- [Rust Cookbook - 生成随机数](https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/random.html)
- [为什么随机数在计算机上是伪随机的？](https://www.freecodecamp.org/news/why-computer-generated-random-numbers-are-not-really-random)
- [Rust中的其他常用库](https://awesome-rust.com)

## 了解更多有关随机数的文章

- [如何使用Rust编写一个随机密码生成器](https://www.digitalocean.com/community/tutorials/how-to-generate-random-passwords-in-rust)
- [如何在Rust中分析随机数生成器的性能](https://medium.com/@federicoterzi/performance-of-random-number-generators-in-rust-55a7c1a3a336)
- [Rust社区对随机数生成的讨论](https://users.rust-lang.org/t/random-vs-crypto-random/10416)