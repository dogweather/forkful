---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何处和为何？

生成随机数就是利用计算机程序来生成预定范围内的随机数。程序员之所以需要生成随机数，纯粹是为了保证数据的非规律性或者应对需要完全随机操作的情况。

## 如何操作：

在 Rust 程序设计语言中，我们可以使用 `rand::Rng` trait 和 `rand::thread_rng` 函数生成随机数。下面是一个简单的例子：

```Rust
use rand::Rng;

fn main() {
    let num = rand::thread_rng().gen_range(0..100);
    println!("Random number: {}", num);
}
```

在这个例子中，我们生成了一个在0到100范围内的随机数，然后将其打印出来。

## 深入探索：

随机数生成器从计算机科学的早期就已经存在，主要用于要求复杂性和无规则性的科学计算和密码学。在Rust语言中生成随机数，我们通常选择利用 `rand` 库中的 `rand::Rng` trait 和 `rand::thread_rng` 函数，但实际上还有其他的方式，比如使用 `OsRng` 或者 `StdRng` 等。

在有些情况下，我们可能需要产生随机种子，这个时候可以考虑使用 `SeedableRng` trait。而在 `rand` 库内部，随机数是通过使用特定的算法（比如线性同余算法等）产生的。

## 更多参考

如果你对Rust的随机数生成器产生兴趣，请访问以下链接以获取更多信息：

- [Rand 库在 Github 上的源码](https://github.com/rust-random/rand)
- [关于随机数生成的维基百科条目](https://en.wikipedia.org/wiki/Random_number_generation)