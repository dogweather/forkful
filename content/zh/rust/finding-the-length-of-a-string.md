---
title:                "Rust: 找到字符串的长度"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么要学习如何计算字符串长度？

Rust是一种强大的编程语言，具有出色的性能和安全性。学习如何计算字符串长度可以帮助您更好地掌握Rust的字符串处理功能，从而提高编程能力和效率。

## 如何计算字符串长度

计算字符串长度的方法有多种，但是在Rust中可以通过使用`str`类型的`len()`方法来实现。首先，我们需要声明一个字符串变量，然后使用`len()`方法来获取字符串的长度，如下所示：

```Rust
let string = "Hello, world!";
let length = string.len();
println!("The length of the string is {}", length);
```
输出结果为：
```
The length of the string is 13
```

除了使用`len()`方法外，还可以通过迭代字符串的每个字符，并使用`count()`方法来计算字符串的长度。示例如下所示：

```Rust
let string = String::from("Hello, world!");
let length = string.chars().count();
println!("The length of the string is {}", length);
```
输出结果同样为13。

## 深入了解字符串长度计算

在Rust中，字符串由`str`和`String`类型表示，它们具有不同的性质。`str`类型是固定大小且不可修改的，而`String`类型是可变且可以自由增长的。因此，在计算字符串长度时需要注意类型的不同。

另外，Rust中的字符串长度是以字节为单位，而不是字符。这意味着某些字符串可能会占用更多的字节，例如含有特殊符号或非ASCII字符的语言，因此计算长度时需要格外注意。

# 查看更多资源

- [Rust官方文档](https://www.rust-lang.org/zh-CN/)
- [Rust语言中文社区](https://rust.cc/)
- [Rust编程语言入门教程](https://wiki.jikexueyuan.com/project/rust-primer/)
- [Rust中文教程](https://github.com/woshiluodi/learingrust)
- [Rust编程语言中文社区](https://rust-china.org/)