---
date: 2024-01-20 17:48:16.987993-07:00
description: "\u5B57\u7B26\u4E32\u957F\u5EA6\u6307\u7684\u662F\u5B57\u7B26\u4E32\u5305\
  \u542B\u7684\u5B57\u7B26\u6570\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u9700\u8981\u77E5\
  \u9053\u8FD9\u4E2A\u6765\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\u6BD4\u5982\u9A8C\
  \u8BC1\u8F93\u5165\u957F\u5EA6\u6216\u8005\u5728\u754C\u9762\u4E0A\u6B63\u786E\u6392\
  \u7248\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.273619-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u957F\u5EA6\u6307\u7684\u662F\u5B57\u7B26\u4E32\u5305\
  \u542B\u7684\u5B57\u7B26\u6570\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u9700\u8981\u77E5\
  \u9053\u8FD9\u4E2A\u6765\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\u6BD4\u5982\u9A8C\
  \u8BC1\u8F93\u5165\u957F\u5EA6\u6216\u8005\u5728\u754C\u9762\u4E0A\u6B63\u786E\u6392\
  \u7248\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
字符串长度指的是字符串包含的字符数。程序员通常需要知道这个来处理文本数据，比如验证输入长度或者在界面上正确排版。

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
