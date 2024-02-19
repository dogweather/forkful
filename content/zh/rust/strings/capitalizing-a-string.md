---
aliases:
- /zh/rust/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:24.566972-07:00
description: "\u5728 Rust \u4E2D\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u6D89\
  \u53CA\u4FEE\u6539\u5B57\u7B26\u4E32\uFF0C\u4F7F\u5176\u7B2C\u4E00\u4E2A\u5B57\u7B26\
  \uFF08\u5982\u679C\u662F\u5B57\u6BCD\u7684\u8BDD\uFF09\u4E3A\u5927\u5199\uFF0C\u540C\
  \u65F6\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u8FD9\u9879\u64CD\u4F5C\u662F\u51FA\u4E8E\
  \u683C\u5F0F\u5316\u7684\u76EE\u7684\uFF0C\u6BD4\u5982\u4E3A\u6807\u9898\u51C6\u5907\
  \u5355\u8BCD\u6216\u786E\u4FDD\u7528\u6237\u8F93\u5165\u7684\u4E00\u81F4\u6027\u3002"
lastmod: 2024-02-18 23:08:58.926713
model: gpt-4-0125-preview
summary: "\u5728 Rust \u4E2D\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u6D89\
  \u53CA\u4FEE\u6539\u5B57\u7B26\u4E32\uFF0C\u4F7F\u5176\u7B2C\u4E00\u4E2A\u5B57\u7B26\
  \uFF08\u5982\u679C\u662F\u5B57\u6BCD\u7684\u8BDD\uFF09\u4E3A\u5927\u5199\uFF0C\u540C\
  \u65F6\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u8FD9\u9879\u64CD\u4F5C\u662F\u51FA\u4E8E\
  \u683C\u5F0F\u5316\u7684\u76EE\u7684\uFF0C\u6BD4\u5982\u4E3A\u6807\u9898\u51C6\u5907\
  \u5355\u8BCD\u6216\u786E\u4FDD\u7528\u6237\u8F93\u5165\u7684\u4E00\u81F4\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
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
