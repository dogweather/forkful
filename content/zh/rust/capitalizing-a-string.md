---
title:                "Rust: 字符串大写化"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么要使用 Rust 进行字符串大写？
对于很多程序员来说，Rust 是一种强大的编程语言，它拥有速度快、内存安全和并发能力等众多优点。当我们需要对字符串进行某种操作时，通常会使用 Rust 来保证代码的高效和可靠性。本文将向您介绍如何使用 Rust 来进行字符串大写操作，并深入探讨其中的原理。

## 如何做到？
在 Rust 中，要对字符串进行大写操作，我们可以使用它提供的标准库中的 `to_uppercase` 方法。该方法可以将字符串中的所有字符转换为大写，并返回一个新的字符串。让我们来看一个简单的示例：

```Rust
let str = "hello world";
let upper_str = str.to_uppercase();

println!("原始字符串: {}", str); // hello world
println!("大写字符串: {}", upper_str); // HELLO WORLD
```

上面的例子中，我们首先将一个字符串赋值给 `str` 变量，然后使用 `to_uppercase()` 方法创建一个新的大写字符串并赋值给 `upper_str` 变量。最后，通过 `println!` 宏输出两个字符串的值，可以看到字符串已被成功转换为大写。

除了使用 `to_uppercase()` 方法，我们还可以使用 `to_ascii_uppercase()` 方法来对字符串进行大写操作。两者的区别在于 `to_ascii_uppercase()` 只会转换 ASCII 字符，而 `to_uppercase()` 则会转换所有字符。让我们来看一个示例：

```Rust
let str = "你好，世界";
let upper_str1 = str.to_uppercase();
let upper_str2 = str.to_ascii_uppercase();

println!("原始字符串: {}", str); // 你好，世界
println!("大写字符串（所有字符）: {}", upper_str1); // 你好，世界被转换为大写后仍为“你好，世界”
println!("大写字符串（ASCII 字符）: {}", upper_str2); // 你好，世界被转换为大写后变为“??，???”
```

如上所示，`to_uppercase()` 方法并没有改变原始字符串中的字符，而 `to_ascii_uppercase()` 则只改变了字符串中的 ASCII 字符。

## 深入探讨
在 Rust 中，字符串是 `String` 类型的变量，而 `String` 类型的变量实际上是一个指向存储内存空间的指针，所以对字符串进行操作实际上是在对指针指向的内存空间进行操作。在 `to_uppercase()` 方法中，Rust 会根据系统的编码方式，判断出每个字符的 ASCII 码，然后将小写字母转换为对应的大写字母，最后组合成一个新的字符串返回。这也解释了为什么 `to_uppercase()` 方法只会转换 ASCII 字符。

值得注意的是，字符串在 Rust 中是不可变的，也就是说，我们不能直接修改一个字符串的值。当我们使用 `to_uppercase()` 方法时，它会创建一个新的字符串并返回，原始的字符串仍然保持不变。这种设计可以保证字符串的安全性，避免出现意外的修改。

# 参考链接
- [Rust 官方文档](https://www.rust-lang.org/)
- [Rust 格式化字符串](https://doc.rust-lang.org/std/fmt/)
- [Rust 标准库 String 文档](https://doc.rust-lang.org/std/string/)
- [Rust 字符串转换为大写](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)

# 参见
- [Rust 字符串切割](https://shimo.im/docs/JWQPY6YTYVrK6KfY/)
- [如何遍历字符串中的每个字符？](