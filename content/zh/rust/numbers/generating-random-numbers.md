---
date: 2024-01-27 20:35:17.534438-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust \u4F9D\u8D56\u5916\u90E8\u7BB1\uFF08\
  crate\uFF09\u6765\u751F\u6210\u968F\u673A\u6570\uFF0C\u5176\u4E2D `rand` \u662F\u6700\
  \u5E38\u7528\u7684\u3002\u8981\u5F00\u59CB\u751F\u6210\u968F\u673A\u6570\uFF0C\u4F60\
  \u9996\u5148\u9700\u8981\u5728\u4F60\u7684 `Cargo.toml` \u6587\u4EF6\u4E2D\u6DFB\
  \u52A0 `rand`\uFF1A."
lastmod: '2024-04-05T21:53:47.837111-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
Rust 依赖外部箱（crate）来生成随机数，其中 `rand` 是最常用的。要开始生成随机数，你首先需要在你的 `Cargo.toml` 文件中添加 `rand`：

```toml
[dependencies]
rand = "0.8.5"
```

接下来，你可以在 Rust 代码中使用 `rand` 生成随机数。这是生成随机整数和浮点数的示例：

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // 生成一个 1 到 10 之间的随机整数
    let random_int: i32 = rng.gen_range(1..=10);
    println!("随机整数: {}", random_int);
    
    // 生成一个 0.0 到 1.0 之间的随机浮点数
    let random_float: f64 = rng.gen::<f64>();
    println!("随机浮点数: {}", random_float);
}
```

示例输出可能是：

```plaintext
随机整数: 7
随机浮点数: 0.9401077112175732
```

注意，重新运行程序将产生不同的值。

## 深入探讨
通过 `rand` 及其依赖项（如 `getrandom`）促成的 Rust 中的随机数生成，代表了对操作系统设施和算法生成器的广泛抽象。从历史上看，计算中的随机性已经从简单、可预测的算法进化到复杂、密码学安全的方法。Rust 通过其可插拔的 `Rng` 特质封装了这一进化，这个特质可以根据所需的随机性质量和性能由各种生成器支持。

对于大多数应用程序，依赖 `rand` 和系统的 RNG 提供了简单性与熵之间的良好平衡。然而，对于密码学应用程序，`rand` 箱依赖 `getrandom` 用于种子生成，后者依赖于操作系统特定机制（例如，在类 Unix 系统上是 `/dev/urandom`），确保了密码学安全的随机性。

另外，如果你有 `rand` 未满足的特定需求，探索其他箱或基于数学模型实现自定义生成器可能是一个途径。尽管如此，对于绝大多数用例，`rand` 及其生态系统提供了既高效又易于整合到 Rust 应用程序的强大解决方案。
