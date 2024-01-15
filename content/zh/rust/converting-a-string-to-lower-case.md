---
title:                "将字符串转换为小写"
html_title:           "Rust: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

当你在开发使用字符串的程序时，有可能会遇到需要将字符串转换为小写的情况。通过这篇文章，你将学习如何用Rust语言来实现这一功能。

## 如何做

首先，我们需要导入标准库中的 `str` 模块。

```Rust
use std::str;
```

接下来，我们可以使用 `to_lowercase()` 方法来将字符串转换为小写。

```Rust
let example_string = "Hello, World!";
let lower_case_string = str::to_lowercase(example_string);
```

下面是完整的代码和输出示例：

```Rust
use std::str;

fn main() {
    let example_string = "Hello, World!";
    let lower_case_string = str::to_lowercase(example_string);
    
    println!("{}", lower_case_string);
}
```

输出：

```bash
hello, world!
```

## 深入探讨

在 Rust 中，字符串是一个 `String` 类型的变量，它默认是不可变的。因此，我们不能直接修改字符串的大小写，而是需要创建一个新的字符串来存储转换的结果。这就是为什么我们在上面的例子中使用 `str::to_lowercase()` 方法。

此外，`to_lowercase()` 方法并不是仅适用于英文字符，它也可以支持 Unicode 字符集。

## 参考链接

- [Rust官方文档 - str::to_lowercase()](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Rust编程语言 - 处理字符串](https://kaisery.github.io/trpl-zh-cn/ch08-02-strings.html)
- [Rust标准库 - 字符串操作](https://doc.rust-lang.org/std/str/)

## 参考

- [为什么要使用 Rust？初学者指南](https://juejin.im/post/5a068bbc5188255de3435360)
- [Rust语言入门指南](https://juejin.im/post/5cc978f2f265da03aa34e2a0)