---
date: 2024-01-20 17:48:16.987993-07:00
description: "How to: \u600E\u4E48\u505A \u5B57\u7B26\u4E32\u957F\u5EA6\u5728\u5386\
  \u53F2\u4E0A\u66FE\u7B80\u5355\u5730\u57FA\u4E8E\u5B57\u8282\uFF0C\u8FD9\u5728ASCII\u6587\
  \u672C\u4E2D\u662F\u6709\u6548\u7684\u3002\u4F46\u73B0\u5728\u5B57\u7B26\u4E32\u5305\
  \u542B\u591A\u79CD\u8BED\u8A00\u548C\u7B26\u53F7\uFF0C\u6240\u4EE5\u5B83\u4EEC\u88AB\
  \u7F16\u7801\u6210\u66F4\u590D\u6742\u7684\u683C\u5F0F\uFF0C\u6BD4\u5982UTF-8\u3002\
  \u5728Rust\u4E2D\uFF0C\u9ED8\u8BA4\u7684\u5B57\u7B26\u4E32\u7C7B\u578B`String`\u662F\
  UTF-8\u7F16\u7801\u7684\u3002\u4E3A\u4E86\u6B63\u786E\u5904\u7406\u591A\u6587\u5316\
  \u6587\u672C\uFF0C\u6211\u4EEC\u9700\u8981\u57FA\u4E8E\u5B57\u7B26\u800C\u4E0D\u662F\
  \u5B57\u8282\u6765\u8003\u8651\u5B57\u7B26\u4E32\u957F\u5EA6\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.671456-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u5B57\u7B26\u4E32\u957F\u5EA6\u5728\u5386\u53F2\u4E0A\
  \u66FE\u7B80\u5355\u5730\u57FA\u4E8E\u5B57\u8282\uFF0C\u8FD9\u5728ASCII\u6587\u672C\
  \u4E2D\u662F\u6709\u6548\u7684\u3002\u4F46\u73B0\u5728\u5B57\u7B26\u4E32\u5305\u542B\
  \u591A\u79CD\u8BED\u8A00\u548C\u7B26\u53F7\uFF0C\u6240\u4EE5\u5B83\u4EEC\u88AB\u7F16\
  \u7801\u6210\u66F4\u590D\u6742\u7684\u683C\u5F0F\uFF0C\u6BD4\u5982UTF-8\u3002\u5728\
  Rust\u4E2D\uFF0C\u9ED8\u8BA4\u7684\u5B57\u7B26\u4E32\u7C7B\u578B`String`\u662FUTF-8\u7F16\
  \u7801\u7684\u3002\u4E3A\u4E86\u6B63\u786E\u5904\u7406\u591A\u6587\u5316\u6587\u672C\
  \uFF0C\u6211\u4EEC\u9700\u8981\u57FA\u4E8E\u5B57\u7B26\u800C\u4E0D\u662F\u5B57\u8282\
  \u6765\u8003\u8651\u5B57\u7B26\u4E32\u957F\u5EA6\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: 怎么做
```Rust
fn main() {
    let greeting = "你好，世界！";
    let length = greeting.chars().count();  // Unicode字符数量
    println!("The length of the string is: {}", length);
}
```
输出：
```
The length of the string is: 6
```

注意：`chars().count()`计算的是Unicode字符的数量，这对于包含非ASCII字符的字符串很重要。

## Deep Dive 深入探索
字符串长度在历史上曾简单地基于字节，这在ASCII文本中是有效的。但现在字符串包含多种语言和符号，所以它们被编码成更复杂的格式，比如UTF-8。在Rust中，默认的字符串类型`String`是UTF-8编码的。为了正确处理多文化文本，我们需要基于字符而不是字节来考虑字符串长度。

Rust还提供了其他方法来测量字符串长度。`len()`方法返回字节长度，这在处理原始字节数据时有用。让我们比较一下：

```Rust
fn main() {
    let ascii = "Hello";
    let multi_lang = "你好，世界！";
    
    println!("ASCII length: {}", ascii.len());  // 字节长度
    println!("Multi-lang chars count: {}", multi_lang.chars().count());
    println!("Multi-lang bytes length: {}", multi_lang.len());
}
```
输出：
```
ASCII length: 5
Multi-lang chars count: 6
Multi-lang bytes length: 21
```

字符和字节长度的差异揭示了使用`.chars().count()`的重要性，特别是当处理多语言场景时。

## See Also 参考链接
- Rust 文档上的字符串类型 [Rust String Docs](https://doc.rust-lang.org/std/string/struct.String.html)
- UTF-8编码介绍 [Understanding UTF-8](http://utf8everywhere.org/)
- 字符迭代器文档 [chars() Iterator](https://doc.rust-lang.org/std/primitive.str.html#method.chars)
