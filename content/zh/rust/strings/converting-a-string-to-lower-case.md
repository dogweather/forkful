---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:39:06.385268-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

把字符串转换成小写意味着将所有字符改为小写格式。程序员这么做通常是为了实现大小写不敏感的比较或搜索。

## How to: (如何操作：)

```Rust
fn main() {
    let greeting = "Hello, World!";
    let lower_greeting = greeting.to_lowercase();

    println!("Original: {}", greeting);
    println!("Lowercase: {}", lower_greeting);
}
```

输出:
```
Original: Hello, World!
Lowercase: hello, world!
```

## Deep Dive (深入探索)

在 Rust 中，`.to_lowercase()` 方法是基于 Unicode 标准实现的。它不仅仅是将 ASCII 字符转换，还能处理其他语言的字符。

早期编程语言对字符串的操作较为基础，但随着 Unicode 的出现，现代语言如 Rust 提供了更全面的文本处理方式。

你也可以使用 `.to_ascii_lowercase()`，它只处理 ASCII 字符，并且效率更高。但如果要考虑国际化，推荐使用 `.to_lowercase()`。

注意，有些语言的大小写转换不是一对一的，所以在某些情况下， `.to_lowercase()` 之后的字符串可能比原来更长。

## See Also (另见)

- Rust 文档中的 [`to_lowercase` 方法](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- Unicode 标准说明: [Unicode Case Folding](https://www.unicode.org/reports/tr44/#CaseFolding)
- 相关函数: [`.to_uppercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
