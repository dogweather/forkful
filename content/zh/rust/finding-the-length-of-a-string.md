---
title:                "寻找字符串的长度"
html_title:           "Rust: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

一直以来，有一项很常见的任务是找出字符串的长度。这项任务对于编程来说尤其重要，因为它涉及到处理用户输入、操作文本内容等各种情况。在Rust中，我们可以通过一些简单的代码来实现这个任务，并且它还是一种很有效的方式。

## 如何

```Rust
let str = "Hello Rust"; // 创建一个字符串变量
let len = str.len(); // 使用.len()方法来获取字符串的长度
println!("The length of the string is {}", len); // 输出结果：The length of the string is 11
```

以上就是使用Rust来找出字符串长度的基本方式。我们首先创建一个字符串变量，然后使用.len()方法来获取它的长度，最后可以按照需要来输出结果。值得注意的是，Rust中的字符串是UTF-8编码的，所以.len()方法返回的是字节数而不是字符数。

## 深入讨论

在Rust中，字符串其实是一个字符的序列，每一个字符都有对应的编码。所以当我们想要获取字符串的长度时，实际上是想要获取其中字符的数量。为了实现这一点，Rust中的字符串类型(String)提供了一个.len()方法来计算字符的数量。

同时，Rust中也提供了bytes()方法和chars()方法来分别获取字符串的字节数和字符数。这些方法可以帮助我们更灵活地操作字符串，满足不同的需求。

## 参考资料

- [Rust官方文档](https://www.rust-lang.org/zh-CN/)
- [Rust Programming Language中文社区](https://rustlang-cn.org/)

## 参见

- [Rust中的常用字符串方法](https://github.com/rust-lang/rust/blob/stable/src/libcore/str/mod.rs)
- [Rust中的字符串类型](https://doc.rust-lang.org/std/string/index.html)