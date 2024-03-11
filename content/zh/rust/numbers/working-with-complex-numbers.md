---
date: 2024-01-26 04:45:35.990806-07:00
description: "\u590D\u6570\u6709\u5B9E\u90E8\u548C\u865A\u90E8\uFF0C\u5BF9\u4E8E\u5DE5\
  \u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u8BA1\u7B97\u673A\u56FE\u5F62\u5B66\u7B49\
  \u591A\u4E2A\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u4EEC\u6765\u89E3\u51B3\u666E\u901A\u5B9E\u6570\u65E0\u6CD5\u5904\u7406\u7684\
  \u65B9\u7A0B\u3002"
lastmod: '2024-03-11T00:14:21.277026-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u6709\u5B9E\u90E8\u548C\u865A\u90E8\uFF0C\u5BF9\u4E8E\u5DE5\
  \u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u8BA1\u7B97\u673A\u56FE\u5F62\u5B66\u7B49\
  \u591A\u4E2A\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u4EEC\u6765\u89E3\u51B3\u666E\u901A\u5B9E\u6570\u65E0\u6CD5\u5904\u7406\u7684\
  \u65B9\u7A0B\u3002"
title: "\u5904\u7406\u590D\u6570"
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
