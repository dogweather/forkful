---
date: 2024-01-20 17:35:42.085746-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u6309\u987A\u5E8F\u7EC4\u5408\u6210\u4E00\u4E2A\u65B0\u4E32\u3002\u7F16\u7A0B\
  \u91CC\uFF0C\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u751F\u6210\u52A8\u6001\
  \u5185\u5BB9\u6216\u8005\u6784\u5EFA\u590D\u6742\u7684\u8F93\u51FA\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.074298-07:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u6309\u987A\u5E8F\u7EC4\u5408\u6210\u4E00\u4E2A\u65B0\u4E32\u3002\u7F16\u7A0B\
  \u91CC\uFF0C\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u751F\u6210\u52A8\u6001\
  \u5185\u5BB9\u6216\u8005\u6784\u5EFA\u590D\u6742\u7684\u8F93\u51FA\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
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
