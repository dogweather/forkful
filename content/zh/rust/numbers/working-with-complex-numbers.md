---
title:                "处理复数"
aliases:
- /zh/rust/working-with-complex-numbers/
date:                  2024-01-26T04:45:35.990806-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
复数有实部和虚部，对于工程学、物理学和计算机图形学等多个领域至关重要。程序员使用它们来解决普通实数无法处理的方程。

## 如何操作：
Rust 没有内建的复数支持，但是像 `num-complex` 这样的库为你提供了支持。以下是如何使用它的方法：

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Sum: {}", sum); // 和: 3 - 1i
    println!("Product: {}", product); // 乘积: 14 - 5i
}
```
你需要在你的 `Cargo.toml` 中添加 `num_complex` 以实现这一魔法。

## 深入了解
复数在 16 世纪被构想出来，但直到 18 世纪，当数学家如欧拉开始研究它们时，复数才真正流行起来。

由于没有原生复数操作，像 Rust 这样的语言依赖第三方库。`num-complex` 就是这样一个库，并且是 `num` 库集合的一部分，旨在为 Rust 提供数字类型和特质。

值得一提的是，一些语言（如 Python）具有内建的复数支持，而其他语言（如 C++，通过 `<complex>` 头文件）将它们作为标准库的一部分提供。在 Rust 中，保持标准库小巧的决定意味着你经常需要寻找社区创建的库以增加额外的功能。

## 参见
- [Rust 书籍](https://doc.rust-lang.org/book/): 了解更多关于 Rust 以及如何使用外部库的信息。
- [复数维基百科](https://en.wikipedia.org/wiki/Complex_number): 深入理解复数。
