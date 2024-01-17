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

# 什么是字符串长度
字符串长度是指字符串中包含的字符的数量。程序员需要找到字符串的长度来执行不同的操作，例如检查字符串是否为空，截取特定的字符，或者在比较字符串时确定它们是否相等。

## 如何找到字符串长度
在Rust中，我们可以使用 `len()` 方法来找到字符串的长度。这个方法会返回一个表示字符串长度的整数值。让我们来看一个例子：

```Rust
let my_string = "Hello World";
let length = my_string.len();

println!("The length of my_string is {}", length);
```

输出结果应该是 `The length of my_string is 11`，因为这个字符串包含了11个字符（包括空格）。

如果你使用的是可变的字符串类型 `String`，也可以使用 `len()` 方法来找到它的长度。例如：

```Rust
let mut my_string = String::from("Hello");
my_string.push_str(" World");

println!("The length of my_string is {}", my_string.len());
```

输出结果仍然是 `The length of my_string is 11`，因为这是两个字符串合并后的总长度。

## 深入了解
字符串的长度操作在编程中是非常常见的，因此在Rust中也有很多其他的方法来获取字符串的长度。其中一个是使用 `byte_len()` 方法，它返回字符串中的字节数。这在处理Unicode字符时可能会更有用。

如果你想知道更多关于Rust中的字符串操作，请查看官方文档中关于 `String` 和 `&str` 类型的部分。此外，你也可以学习一些有用的字符串处理库，例如 `std::string::String` 和 `std::str`。

## 参考资料
- [Rust官方文档 - String](https://doc.rust-lang.org/std/string/)
- [Rust官方文档 - &str](https://doc.rust-lang.org/std/str/)