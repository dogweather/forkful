---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"

category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
正则表达式用于文本搜索、替换、验证等操作。程序员使用它们因为它们强大、高效、灵活。

## How to: (如何做)
```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b\w{5}\b").unwrap(); // 查找所有五个字母的单词
    let text = "Rust语言简洁又强大。";

    for word in re.find_iter(&text) {
        println!("找到单词: {}", word.as_str());
    }
}
```
输出:
```
找到单词: 语言
找到单词: 简洁
```

## Deep Dive (深入探究)
1. 正则表达式起源于1950年代的神经生物学研究。
2. 替代方案包括字符串搜索（更简单但功能有限）和解析库（更复杂）。
3. Rust中正则表达式的实现被设计为快速且安全。

## See Also (另请参阅)
- Rust 的正则表达式库官方文档: [docs.rs/regex](https://docs.rs/regex/)
- 正则表达式学习资源: [regex101.com](https://regex101.com/)
- Rust 编程语言书: [doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
