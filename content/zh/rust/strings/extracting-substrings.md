---
date: 2024-01-20 17:46:32.322757-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u5B57\u7B26\u4E32\
  \u4E2D\u83B7\u53D6\u7279\u5B9A\u90E8\u5206\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6570\u636E\u5904\u7406\u3001\u5206\u6790\u6587\
  \u672C\u6216\u8005\u8403\u53D6\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.071046-07:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u5B57\u7B26\u4E32\
  \u4E2D\u83B7\u53D6\u7279\u5B9A\u90E8\u5206\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6570\u636E\u5904\u7406\u3001\u5206\u6790\u6587\
  \u672C\u6216\u8005\u8403\u53D6\u4FE1\u606F\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## 什么 & 为什么？
提取子字符串就是从字符串中获取特定部分的过程。程序员这么做是为了数据处理、分析文本或者萃取信息。

## 如何：
```Rust
fn main() {
    let s = String::from("你好，世界！");
    let hello = &s[0..6];        // 注意：这是按字节索引的！
    
    println!("提取的子字符串: {}", hello);    // 输出：你好，
}

// 注意：当运行代码时，我们假设你使用的是 UTF-8 编码的文本。
```

## 混水摸鱼
在历史上，不同编程语言处理子字符串的方式多种多样，Rust 特别关注安全和性能，所以它使用字节索引进行操作。这意味着你需要特别注意文本编码（通常是 UTF-8）。另外，Rust 的 `String` 类型确保存储的是有效的 UTF-8 序列，这样就不用担心无效编码的问题。还有其他库如 `substring` 可以简化操作，但内置方法已足够大部分场景。

## 另请参阅
- [Rust 字符串切片](https://doc.rust-lang.org/std/string/struct.String.html#method.get)
- [Rust 如何处理 UTF-8](https://doc.rust-lang.org/std/str/)
