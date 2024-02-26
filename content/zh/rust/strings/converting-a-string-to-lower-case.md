---
date: 2024-01-20 17:39:06.385268-07:00
description: "\u628A\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u610F\u5473\u7740\
  \u5C06\u6240\u6709\u5B57\u7B26\u6539\u4E3A\u5C0F\u5199\u683C\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u5B9E\u73B0\u5927\u5C0F\u5199\
  \u4E0D\u654F\u611F\u7684\u6BD4\u8F83\u6216\u641C\u7D22\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.068398-07:00'
model: gpt-4-1106-preview
summary: "\u628A\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u610F\u5473\u7740\
  \u5C06\u6240\u6709\u5B57\u7B26\u6539\u4E3A\u5C0F\u5199\u683C\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u5B9E\u73B0\u5927\u5C0F\u5199\
  \u4E0D\u654F\u611F\u7684\u6BD4\u8F83\u6216\u641C\u7D22\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
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
