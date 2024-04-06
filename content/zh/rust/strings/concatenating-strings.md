---
date: 2024-01-20 17:35:42.085746-07:00
description: "How to: Rust\u7684\u5B57\u7B26\u4E32\u62FC\u63A5\uFF0C\u76F8\u6BD4\u5386\
  \u53F2\u4E0A\u7684\u4E00\u4E9B\u7F16\u7A0B\u8BED\u8A00\uFF0C\u8BBE\u8BA1\u5F97\u66F4\
  \u6CE8\u91CD\u6027\u80FD\u548C\u5B89\u5168\u3002Rust \u907F\u514D\u4E86\u9690\u5F0F\
  \u7684\u5185\u5B58\u590D\u5236\uFF0C\u6240\u6709\u6743\u7CFB\u7EDF\u786E\u4FDD\u5B57\
  \u7B26\u4E32\u5728\u4F7F\u7528\u540E\u80FD\u6B63\u5F53\u91CA\u653E\u3002\u9664\u4E86\
  \u4F7F\u7528 `+` \u8FD0\u7B97\u7B26\u548C `format!` \u5B8F\uFF0C\u4F60\u8FD8\u53EF\
  \u4EE5\u5229\u7528\u8FED\u4EE3\u5668\u914D\u5408 `collect` \u65B9\u6CD5\u6216\u8005\
  \u4F7F\u7528 `push` \u548C `push_str`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.832848-06:00'
model: gpt-4-1106-preview
summary: "Rust\u7684\u5B57\u7B26\u4E32\u62FC\u63A5\uFF0C\u76F8\u6BD4\u5386\u53F2\u4E0A\
  \u7684\u4E00\u4E9B\u7F16\u7A0B\u8BED\u8A00\uFF0C\u8BBE\u8BA1\u5F97\u66F4\u6CE8\u91CD\
  \u6027\u80FD\u548C\u5B89\u5168\u3002Rust \u907F\u514D\u4E86\u9690\u5F0F\u7684\u5185\
  \u5B58\u590D\u5236\uFF0C\u6240\u6709\u6743\u7CFB\u7EDF\u786E\u4FDD\u5B57\u7B26\u4E32\
  \u5728\u4F7F\u7528\u540E\u80FD\u6B63\u5F53\u91CA\u653E\u3002\u9664\u4E86\u4F7F\u7528\
  \ `+` \u8FD0\u7B97\u7B26\u548C `format!` \u5B8F\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u5229\
  \u7528\u8FED\u4EE3\u5668\u914D\u5408 `collect` \u65B9\u6CD5\u6216\u8005\u4F7F\u7528\
  \ `push` \u548C `push_str` \u65B9\u6CD5\u76F4\u63A5\u5728\u5B57\u7B26\u4E32\u4E0A\
  \u6DFB\u52A0\u5B57\u7B26\u6216\u5B50\u4E32\u3002\u6BCF\u79CD\u65B9\u6CD5\u90FD\u6709\
  \u5B83\u7684\u4F7F\u7528\u573A\u666F\uFF0C\u9009\u62E9\u54EA\u4E2A\u53D6\u51B3\u4E8E\
  \u5177\u4F53\u9700\u6C42\u548C\u6027\u80FD\u8003\u91CF\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
