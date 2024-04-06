---
date: 2024-01-20 17:55:11.337699-07:00
description: "How to: \u600E\u4E48\u505A\uFF1A Rust \u4ECE\u8BDE\u751F\u4E4B\u521D\
  \u5C31\u6709\u4E86\u5904\u7406\u6587\u4EF6I/O\u7684\u80FD\u529B\u3002\u4E0A\u8FF0\
  \u4EE3\u7801\u662F\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u7684\u57FA\u672C\u65B9\u6CD5\
  \uFF0C\u4F46\u662F\u5386\u53F2\u4E0A\u6211\u4EEC\u4E5F\u6709\u5176\u4ED6\u7684\u9009\
  \u62E9\uFF0C\u6BD4\u5982\u4F7F\u7528`std::fs::read_to_string`\u51FD\u6570\u3002\
  `File::open` \u548C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.860158-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A\uFF1A Rust \u4ECE\u8BDE\u751F\u4E4B\u521D\u5C31\u6709\
  \u4E86\u5904\u7406\u6587\u4EF6I/O\u7684\u80FD\u529B\u3002\u4E0A\u8FF0\u4EE3\u7801\
  \u662F\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u7684\u57FA\u672C\u65B9\u6CD5\uFF0C\u4F46\
  \u662F\u5386\u53F2\u4E0A\u6211\u4EEC\u4E5F\u6709\u5176\u4ED6\u7684\u9009\u62E9\uFF0C\
  \u6BD4\u5982\u4F7F\u7528`std::fs::read_to_string`\u51FD\u6570\u3002`File::open`\
  \ \u548C `read_to_string`\u64CD\u4F5C\u7B80\u5355\uFF0C\u9002\u7528\u5C0F\u6587\u4EF6\
  \u3002\u5927\u6587\u4EF6\u5219\u8003\u8651\u6309\u884C\u8BFB\u53D6\u6216\u8005\u5757\
  \u8BFB\u53D6\u4EE5\u8282\u7701\u5185\u5B58."
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to: 怎么做：
```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("hello.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("File Contents:\n{}", contents);
    Ok(())
}
```
输出样例：
```
File Contents:
Hello, world!
```

## Deep Dive 深入探讨：
Rust 从诞生之初就有了处理文件I/O的能力。上述代码是读取文本文件的基本方法，但是历史上我们也有其他的选择，比如使用`std::fs::read_to_string`函数。`File::open` 和 `read_to_string`操作简单，适用小文件。大文件则考虑按行读取或者块读取以节省内存。

实现详情方面，Rust 保证了类型安全和内存安全，就是说读文件时，如果出错了，程序不会崩溃，它会返回一个`Result`类型让你处理错误。

## See Also 相关资源：
- Rust 书[官方指南]: https://doc.rust-lang.org/stable/book/ch12-02-reading-a-file.html
- [std::fs]: https://doc.rust-lang.org/stable/std/fs/index.html
- [std::io]: https://doc.rust-lang.org/stable/std/io/index.html
- [The Rust Programming Language 练习书]: https://github.com/rust-lang/rustlings/
