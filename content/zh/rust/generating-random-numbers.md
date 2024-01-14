---
title:    "Rust: 生成随机数字"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数是计算机编程中常见的需求，它可以应用在诸如游戏、密码学和模拟等领域。在Rust中，我们可以使用内置的随机数函数来生成高质量的随机数，这将为我们的程序带来更好的安全性和效率。

## 如何实现

要在Rust中生成随机数，我们需要使用标准库中的rand模块。首先，我们需要在程序的开头引入该模块：

```Rust
use rand::Rng;
```

然后，我们可以使用该模块中的随机数生成器来产生随机数。下面是一个生成0到100之间随机数的示例：

```Rust
let mut rng = rand::thread_rng();
let num: u8 = rng.gen_range(0, 100);
println!("随机数为: {}", num);
```

输出可能为："随机数为: 73"。

## 深入探讨

生成高质量的随机数并不是一个简单的过程，它需要保证生成的数能够均匀地分布在所指定的范围内。Rust的rand模块使用了XorShift算法来生成伪随机数序列，该算法具有良好的速度和随机性，并且经过严格的测试来保证生成的随机数质量。在实际应用中，我们也可以通过设置不同的随机数生成器来满足不同的需求，例如产生加密级别的随机数等。

## 参考链接

- [Rust官方文档 - 随机数](https://doc.rust-lang.org/std/rand/index.html)
- [《Rust编程之道》 - 随机数](https://learnku.com/docs/nomicon/2018/random-number/4039)
- [XorShift算法详解](https://en.wikipedia.org/wiki/Xorshift)