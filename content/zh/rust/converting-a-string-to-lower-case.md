---
title:                "Rust: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

为什么要把字符串转换成小写？在编程中，我们经常需要对用户输入的字符串做一些处理，例如比较、搜索或者输出。而将所有的字符串都转换成小写能够简化这些操作，避免大小写带来的混乱。

## 如何操作

Rust提供了一个`to_lowercase`方法，可以将字符串转换成小写。下面是一个简单的例子：

```rust
let str = "Hello, World!";
let lower_case = str.to_lowercase();
println!("{}", lower_case); // 输出 "hello, world!"
```

我们首先定义了一个字符串 `str`，然后使用`to_lowercase`方法将其转换成小写，最后使用`println`来输出结果。你可以运行下面的代码段来查看结果：

```rust
let str = "Hello, World!";
let lower_case = str.to_lowercase();
println!("{}", lower_case); // 输出 "hello, world!"
```

更多关于`to_lowercase`方法的使用方法，可以查看Rust官方文档的说明。

## 深入了解

当我们调用`to_lowercase`方法时，实际上Rust会返回一个新的字符串，而不是直接修改原来的字符串。这是因为Rust的字符串是不可变的，所以我们必须使用`let`关键字重新绑定新的字符串。

此外，在将字符串转换成小写时，Rust会考虑不同语言的字母大小写规则。例如在德语中，字母 "ß" 会被转换成两个小写字母 "ss"。这样做能够保证字符串转换的准确性。

## 查看更多相关内容

如果你想了解更多关于Rust中字符串的操作，可以查看下面的链接：

- [Rust官方文档](https://doc.rust-lang.org/stable/std/string/struct.String.html#method.to_lowercase)
- [《Rust编程之道》字符串相关章节](https://rustwiki.org/zh-CN/rust-by-example/string.html)
- [Rust中文社区的字符串相关讨论](https://rust.cc/article?id=8bdd926d-3f3d-4ff1-ac1e-711a974c597e)