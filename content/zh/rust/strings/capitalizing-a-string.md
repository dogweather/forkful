---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:24.566972-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Rust \u4E2D\u5927\u5199\u5316\
  \u4E00\u4E2A\u5B57\u7B26\u4E32\uFF0C\u4F60\u6709\u4E24\u4E2A\u4E3B\u8981\u9014\u5F84\
  \uFF1A\u4F7F\u7528\u6807\u51C6\u5E93\u529F\u80FD\u6216\u4F7F\u7528\u7B2C\u4E09\u65B9\
  \u5E93\u6765\u6EE1\u8DB3\u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u9700\u6C42\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u505A\u5230\u8FD9\u4E24\u70B9\u3002 Rust \u7684\u6807\
  \u51C6\u5E93\u6CA1\u6709\u63D0\u4F9B\u76F4\u63A5\u5927\u5199\u5316\u5B57\u7B26\u4E32\
  \u7684\u65B9\u6CD5\uFF0C\u4F46\u4F60\u53EF\u4EE5\u901A\u8FC7\u64CD\u4F5C\u5B57\u7B26\
  \u4E32\u7684\u5B57\u7B26\u6765\u5B9E\u73B0\u8FD9\u4E00\u70B9\u3002"
lastmod: '2024-03-13T22:44:47.501661-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u5728 Rust \u4E2D\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\
  \uFF0C\u4F60\u6709\u4E24\u4E2A\u4E3B\u8981\u9014\u5F84\uFF1A\u4F7F\u7528\u6807\u51C6\
  \u5E93\u529F\u80FD\u6216\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u6765\u6EE1\u8DB3\u66F4\
  \u590D\u6742\u6216\u7279\u5B9A\u7684\u9700\u6C42\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\
  \u505A\u5230\u8FD9\u4E24\u70B9."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
要在 Rust 中大写化一个字符串，你有两个主要途径：使用标准库功能或使用第三方库来满足更复杂或特定的需求。以下是如何做到这两点。

### 使用 Rust 的标准库
Rust 的标准库没有提供直接大写化字符串的方法，但你可以通过操作字符串的字符来实现这一点。

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // 输出：Hello
}
```

### 使用 `heck` 库
对于一个更直接的方法，特别是在进行较大的文本处理时，你可能更倾向于使用第三方库，比如 `heck`。`heck` 库提供了各种情况转换功能，包括一种简单的大写化字符串的方法。

首先，将 `heck` 添加到你的 `Cargo.toml` 中：

```toml
[dependencies]
heck = "0.4.0"
```

然后，使用它来大写化你的字符串：

```rust
extern crate heck; // 在 Rust 2018 版本或之后不需要
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // 输出：Hello World
}
```

注意：`heck` 提供的 `to_title_case` 方法会大写化字符串中的每个单词，如果你只想大写化字符串的第一个字符，这可能超出了你的需求。根据你的具体需求调整你的使用方式。
