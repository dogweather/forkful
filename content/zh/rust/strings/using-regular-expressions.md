---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:00.639283-07:00
description: "\u5982\u4F55\u505A\uFF1A Rust\u7684`regex`\u5E93\u662F\u5904\u7406\u6B63\
  \u5219\u8868\u8FBE\u5F0F\u7684\u9996\u9009\u3002\u8981\u4F7F\u7528\u5B83\uFF0C\u4F60\
  \u9996\u5148\u9700\u8981\u5C06\u5B83\u6DFB\u52A0\u5230\u4F60\u7684`Cargo.toml`\u4E2D\
  \uFF1A."
lastmod: '2024-04-05T21:53:47.830913-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

## 如何做：
Rust的`regex`库是处理正则表达式的首选。要使用它，你首先需要将它添加到你的`Cargo.toml`中：

```toml
[dependencies]
regex = "1"
```

然后，你可以开始在你的Rust代码中实现regex功能。以下是执行一些常见操作的方法：

### 匹配字符串中的模式
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("文本是否匹配日期模式？{}", re.is_match(date));
    // 输出：文本是否匹配日期模式？true
}
```

### 查找和访问匹配
```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("语言：{}, 年份：{}", &cap[1], &cap[2]);
    }
    // 输出：
    // 语言：Rust, 年份：2023
    // 语言：C++, 年份：2022
    // 语言：Python, 年份：2021
}
```

### 替换文本
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 在 $2 年更新");

    println!("更新后的文本：{}", replaced);
    // 输出：更新后的文本：Rust 在 2023 年更新, C++ 在 2022 年更新, Python 在 2021 年更新
}
```

### 使用正则表达式分割文本
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // 在任何非单词字符处分割
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("语言：{}", field);
    }
    // 输出：
    // 语言：Rust
    // 语言：C++
    // 语言：Python
    // 语言：Go
}
```

这些示例为你开始使用Rust中的正则表达式提供了基础指南。随着你的需求变得更加复杂，`regex`箱（crate）为复杂的模式匹配和文本操作任务提供了丰富的功能。
