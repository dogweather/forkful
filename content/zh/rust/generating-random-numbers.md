---
title:                "生成随机数"
html_title:           "Rust: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数

生成随机数在编程中扮演着重要的角色，它可以帮助我们模拟真实世界的情况，或者在游戏开发中创建不同的游戏体验。它也可以用于加密，使得数据更加安全。

## 如何生成随机数

```Rust
// 使用标准库中的rand模块
use rand::Rng;

fn main() {
    // 生成一个范围在0-10之间的随机整数
    let random_num = rand::thread_rng().gen_range(0..11);
    // 打印输出
    println!("随机数: {}", random_num);
}
```

输出:

```
随机数: 8
```

除了生成随机整数，我们还可以使用`gen_range`方法来生成随机浮点数，或者使用`choose`方法从一组数据中随机选择元素。

## 深入了解随机数生成

生成随机数的过程实际上是通过使用伪随机数算法来计算出来的。这些算法的特点是每次生成的随机数都是基于该算法的种子（seed）所计算出来的。种子可以是任何随意的值，但是通常我们会使用操作系统提供的硬件随机值来作为种子，这样可以保证生成的随机数更加真实和随机。

## 参考链接

- Rust官方文档: https://doc.rust-lang.org/std/rand/index.html
- Rust编程语言中文社区: https://rust.cc/
- 《Rust编程之道》: https://kaisery.github.io/trpl-zh-cn/
- 随机数生成的原理和应用: https://blog.csdn.net/huang_biao/article/details/79444154