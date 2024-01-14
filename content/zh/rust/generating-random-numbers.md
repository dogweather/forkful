---
title:                "Rust: 产生随机数"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

《为什么用 Rust 生成随机数》

在编程中，有时需要生成随机数。Rust 提供了一个强大的随机数库，可以帮助我们轻松生成高质量的随机数。让我们来看看如何使用 Rust 来生成随机数吧！

## 如何

生成随机数需要使用到 Rust 的标准库中的`rand`模块。首先，我们需要在项目的`Cargo.toml`文件中添加`rand`依赖。

```Rust
[dependencies]
rand = "0.8.3"
```

然后，在代码中导入`rand`模块，并使用`thread_rng()`函数初始化随机数发生器。

```Rust
use rand::{thread_rng, Rng};
```

现在，我们可以使用`gen_range()`函数来生成一个指定范围内的随机数。

```Rust
let random_number = thread_rng().gen_range(1..=10);
println!("随机数为：{}", random_number);
```

运行上面的代码，输出结果可能是：随机数为：7。

## 深入探究

生成随机数的过程是基于伪随机数生成器。这些生成器使用一个叫做种子的数值来生成一系列看似随机的数值。如果使用相同的种子，那么每次生成的随机数序列也将是相同的。

为了避免每次生成的随机数都相同，我们可以使用`thread_rng()`函数，它会自动使用操作系统提供的真随机数作为种子。也可以手动指定不同的种子来获得不同的随机数序列。

另外，我们还可以使用`shuffle()`方法来打乱一个序列中的元素。这在需要随机排序时非常有用。

## 参考链接

- [Rust官方文档-使用随机数](https://doc.rust-lang.org/book/ch07-01-modules.html)
- [Rand官方文档](https://docs.rs/rand/0.8.3/rand/)

# 查看更多

- [Rust官方文档](https://www.rust-lang.org/zh-CN/)
- [Rust中文社区论坛](https://forum.rust-china.org/)
- [Rust编程语言中文站](https://rust-lang-cn.org/)