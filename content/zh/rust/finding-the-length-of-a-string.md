---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

了解字符串的长度是指确定字符串中字符的数量。这对于程序员来说很重要，例如，当需要通过比较字符串的长度来进行某些操作时。

## 如何做：

```Rust
fn main() {
    let hello = "你好"; 
    println!("Length : {}", hello.len());
}
```
运行上述 Rust 代码的输出会给出字符串 "你好" 的长度。
```Rust
Length : 6
```
注意这里的长度是6，而不是2，因为 `len()` 方法计算的是字节的数量，而 "你好" 中的每个字符都以3字节存储。

## 深入探究

在早期的编程语言中，经常通过末尾的 null 字符来确定字符串的长度。但由于 Rust 关注安全性并避免脆弱性，因此采取了不同的方法，即 `len()` 方法，它返回字符串的字节长度。

虽然 `len()` 是最直接的方式来获取字符串的长度，但你也可以选择使用 `chars().count()` 或 `graphemes().count()，当你需要处理 Unicode 字符串时会发现这些方法十分有用。

至于 Rust 为何把字符串长度定义为字节长度，这是因为 Rust 的字符串默认使用 UTF-8 编码。在 UTF-8 中，一个字符的长度可能从 1 字节变到 4 字节，这便是为何 "你好" 由两个字符组成，但长度是6字节的原因。

## 更多信息

你可以在 Rust 官方手册中了解更多信息：
- [The String Type](https://doc.rust-lang.org/stable/book/ch08-02-strings.html)
- [len() method](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [The Chars Type](https://doc.rust-lang.org/std/string/struct.Chars.html)
- [The Graphemes Type](https://unicode-rs.github.io/unicode-segmentation/unicode_segmentation/struct.Graphemes.html)