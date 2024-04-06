---
date: 2024-01-20 17:39:06.385268-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728 Rust \u4E2D\uFF0C`.to_lowercase()`\
  \ \u65B9\u6CD5\u662F\u57FA\u4E8E Unicode \u6807\u51C6\u5B9E\u73B0\u7684\u3002\u5B83\
  \u4E0D\u4EC5\u4EC5\u662F\u5C06 ASCII \u5B57\u7B26\u8F6C\u6362\uFF0C\u8FD8\u80FD\u5904\
  \u7406\u5176\u4ED6\u8BED\u8A00\u7684\u5B57\u7B26\u3002 \u65E9\u671F\u7F16\u7A0B\u8BED\
  \u8A00\u5BF9\u5B57\u7B26\u4E32\u7684\u64CD\u4F5C\u8F83\u4E3A\u57FA\u7840\uFF0C\u4F46\
  \u968F\u7740 Unicode \u7684\u51FA\u73B0\uFF0C\u73B0\u4EE3\u8BED\u8A00\u5982 Rust\
  \ \u63D0\u4F9B\u4E86\u66F4\u5168\u9762\u7684\u6587\u672C\u5904\u7406\u65B9\u5F0F\
  \u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.666404-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728 Rust \u4E2D\uFF0C`.to_lowercase()`\
  \ \u65B9\u6CD5\u662F\u57FA\u4E8E Unicode \u6807\u51C6\u5B9E\u73B0\u7684\u3002\u5B83\
  \u4E0D\u4EC5\u4EC5\u662F\u5C06 ASCII \u5B57\u7B26\u8F6C\u6362\uFF0C\u8FD8\u80FD\u5904\
  \u7406\u5176\u4ED6\u8BED\u8A00\u7684\u5B57\u7B26\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
