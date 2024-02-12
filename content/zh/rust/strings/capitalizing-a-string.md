---
title:                "字符串大写化"
aliases: - /zh/rust/capitalizing-a-string.md
date:                  2024-02-03T19:06:24.566972-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Rust 中大写化一个字符串涉及修改字符串，使其第一个字符（如果是字母的话）为大写，同时保持字符串的其余部分不变。程序员经常进行这项操作是出于格式化的目的，比如为标题准备单词或确保用户输入的一致性。

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
