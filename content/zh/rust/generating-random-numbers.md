---
title:                "生成随机数"
date:                  2024-01-20T17:49:51.048746-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么?)
生成随机数字是创建不可预测值的过程。程序员会这么做为了增加安全性，测试，或者是增加游戏的不可预测性。

## How to: (如何操作:)
在Rust中，随机数生成通常使用`rand`库。这个库提供多种产生随机数的方法。首先，你要在Cargo.toml里添加`rand`依赖。

```toml
[dependencies]
rand = "0.8.3"
```

现在你可以生成一个随机整数了。这是个简单的例子：

```Rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let n: i32 = rng.gen(); // 生成一个随机i32类型的整数
    println!("Random i32: {}", n);
}
```

当你运行它时，它会打印出一个随机数，例如：

```
Random i32: -1728091623
```

## Deep Dive (深入探究)
历史上，随机数生成经常是依赖于硬件，比如噪声、电子随机噪声等。随着计算机编程的发展，许多不同的算法如线性同余发生器等被创建出来。在Rust中，`rand`库利用各种算法和系统提供的资源来生成随机数。

除了基本的随机数生成，`rand`库也支持各种分布，例如均匀分布、正态分布等。

实现上，`rand`库中的`Rng`特征定义了随机数生成器应该具备的方法，而`thread_rng()`为当前线程提供了一个快捷的随机数生成器。

## See Also (另请参阅)
- Rust `rand` 库文档: [https://docs.rs/rand](https://docs.rs/rand)
- Rust 语言官方网站: [https://www.rust-lang.org/](https://www.rust-lang.org/)
- 关于随机数生成的维基百科页面: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
