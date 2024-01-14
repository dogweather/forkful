---
title:    "Rust: 产生随机数"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么要使用 Rust 来生成随机数

Rust 是一种现代，高效的编程语言，它的特点包括内存安全和强大的并发能力。通过使用 Rust 来生成随机数，我们可以利用这些特点来保证生成的随机数具有高质量和可靠性。

## 如何使用 Rust 生成随机数

为了生成随机数，我们首先需要导入 `rand` 库。接着，我们可以使用 `thread_rng` 方法来生成一个随机数生成器，它会重复使用当前线程的随机数种子。然后，我们可以使用不同的方法和数据类型来生成不同范围的随机数。以下是一个简单的例子：

```Rust
use rand::prelude::*;

fn main() {
    // 生成一个随机数生成器
    let mut rng = thread_rng();

    // 生成一个 [0, 10) 范围内的随机整数
    let random_int = rng.gen_range(0, 10);

    // 生成一个 [0, 1) 范围内的随机浮点数
    let random_float = rng.gen_range(0.0, 1.0);

    // 生成一个布尔值
    let random_bool = rng.gen::<bool>();

    // 打印随机数
    println!("Random integer: {}", random_int);
    println!("Random float: {}", random_float);
    println!("Random bool: {}", random_bool);
}
```

运行上述代码，我们可以得到类似下面的输出：

```
Random integer: 6
Random float: 0.718578123114106
Random bool: true
```

## 深入了解生成随机数

Rust 的 `rand` 库提供了广泛的方法和数据类型来生成随机数，包括均匀分布的随机数、正态分布的随机数和指数衰减的随机数等。我们还可以使用 `SeedableRng` trait 来创建自定义的随机数生成器，并使用自己的随机数种子来生成随机数。如果想要更深入地了解 Rust 中的随机数生成，可以阅读官方文档或者编写自己的随机数生成器来体验其中的原理。

## 参考链接

- Rust 官方文档：https://www.rust-lang.org/zh-CN
- `rand` 库文档：https://docs.rs/rand/
- Rust 中的随机数生成器：https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html#creating-a-library-package