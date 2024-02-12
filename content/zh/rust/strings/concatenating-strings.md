---
title:                "字符串拼接"
aliases:
- /zh/rust/concatenating-strings/
date:                  2024-01-20T17:35:42.085746-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
字符串拼接就是将多个字符串按顺序组合成一个新串。编程里，我们这么做是为了生成动态内容或者构建复杂的输出。

## How to:
```Rust
fn main() {
    // 使用 `+` 运算符拼接字符串
    let hello = "你好".to_string();
    let world = "世界!";
    let hello_world = hello + world;
    println!("{}", hello_world); // 输出: 你好世界!

    // 使用 format! 宏来拼接字符串
    let greeting = format!("{} {}", "你好", "世界!");
    println!("{}", greeting); // 输出: 你好 世界!
}
```

## Deep Dive
Rust的字符串拼接，相比历史上的一些编程语言，设计得更注重性能和安全。Rust 避免了隐式的内存复制，所有权系统确保字符串在使用后能正当释放。除了使用 `+` 运算符和 `format!` 宏，你还可以利用迭代器配合 `collect` 方法或者使用 `push` 和 `push_str` 方法直接在字符串上添加字符或子串。每种方法都有它的使用场景，选择哪个取决于具体需求和性能考量。

## See Also
- [Rust 标准库中 `String` 的文档](https://doc.rust-lang.org/std/string/struct.String.html)
- [The Rust Programming Language book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
