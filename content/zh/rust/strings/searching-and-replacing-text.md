---
date: 2024-01-20 17:58:41.208571-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Rust\u4E2D\uFF0C\u6587\
  \u672C\u7684\u641C\u7D22\u548C\u66FF\u6362\u53EF\u4EE5\u4F7F\u7528`str`\u7684`replace`\u65B9\
  \u6CD5\u6765\u5B9E\u73B0\u3002\u5386\u53F2\u4E0A\uFF0C\u6587\u672C\u66FF\u6362\u64CD\
  \u4F5C\u7531\u7F16\u8F91\u5668\u548C\u547D\u4EE4\u884C\u5DE5\u5177\uFF08\u5982`sed`\uFF09\
  \u652F\u6301\u3002Rust\u7684`str`\u548C`String`\u7C7B\u578B\u63D0\u4F9B\u4E86\u591A\
  \u79CD\u66FF\u6362\u65B9\u6CD5\uFF0C\u9002\u5408\u4E0D\u540C\u7684\u573A\u666F\uFF0C\
  \u6BD4\u5982`replace`, `replacen`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.826106-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Rust\u4E2D\uFF0C\u6587\u672C\u7684\
  \u641C\u7D22\u548C\u66FF\u6362\u53EF\u4EE5\u4F7F\u7528`str`\u7684`replace`\u65B9\
  \u6CD5\u6765\u5B9E\u73B0\u3002\u5386\u53F2\u4E0A\uFF0C\u6587\u672C\u66FF\u6362\u64CD\
  \u4F5C\u7531\u7F16\u8F91\u5668\u548C\u547D\u4EE4\u884C\u5DE5\u5177\uFF08\u5982`sed`\uFF09\
  \u652F\u6301\u3002Rust\u7684`str`\u548C`String`\u7C7B\u578B\u63D0\u4F9B\u4E86\u591A\
  \u79CD\u66FF\u6362\u65B9\u6CD5\uFF0C\u9002\u5408\u4E0D\u540C\u7684\u573A\u666F\uFF0C\
  \u6BD4\u5982`replace`, `replacen`, \u548C`replace_range`\u3002\u5B9E\u73B0\u8FD9\
  \u4E9B\u529F\u80FD\u65F6\uFF0CRust\u5229\u7528\u4E86\u6240\u6709\u6743\u548C\u501F\
  \u7528\u68C0\u67E5\u6765\u4FDD\u8BC1\u64CD\u4F5C\u7684\u5B89\u5168\u6027."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: (如何操作：)
```Rust
fn main() {
    let text = "Hello, world! World is wonderful.";
    let updated_text = text.replace("World", "Rust");
    println!("{}", updated_text);
}
```
输出:
```
Hello, Rust! Rust is wonderful.
```

## Deep Dive (深入探讨)
在Rust中，文本的搜索和替换可以使用`str`的`replace`方法来实现。历史上，文本替换操作由编辑器和命令行工具（如`sed`）支持。Rust的`str`和`String`类型提供了多种替换方法，适合不同的场景，比如`replace`, `replacen`, 和`replace_range`。实现这些功能时，Rust利用了所有权和借用检查来保证操作的安全性。

除了标准库方法，还有诸如`regex`这样的crate，提供更强大的搜索和替换功能，能构建复杂的模式匹配和高效批量替换。

## See Also (另请参阅)
- Rust官方文档关于字符串处理：https://doc.rust-lang.org/std/string/struct.String.html
- `regex` crate文档：https://docs.rs/regex/*/regex/
