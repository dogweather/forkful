---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，我们所谓的“提取子串”就是从较大字符串中把特定部分取出来。将字符串分解为更小部分非常有用，例如解析用户输入或处理文件内容。

## 如何做：

在 Rust 中，我们可以使用 slice 操作从字符串中提取子串。让我们看一个例子：

```Rust
fn main() {
    let s = "Hello World!";
    let substring = &s[0..5]; // "Hello"
    println!("{}", substring);
}
```

执行上述代码，你会在控制台上看到输出 "Hello"。

## 深入探索：

Rust 的字符串与许多其他编程语言的字符串不同，因为它们是 UTF-8 编码的。这意味着你不能简单地通过索引来提取子串。例如，如果你尝试获取一个处于 Unicode 字符边界之内的子串，Rust 会报错。这是为了保护你免于无意间创建无效的 UTF-8 字符串。

有其他方法可以提取子串，比如使用 `chars` 或 `bytes` 方法将字符串转换为字符或字节的迭代器，然后对其进行操作。但是通常情况下，slice 操作已经足够使用了。

## 另请参阅：

1. [官方文档对字符串的解释](https://doc.rust-lang.org/book/ch08-02-strings.html#slicing-strings)
2. [在Stack Overflow上关于Rust字符串处理的讨论](https://stackoverflow.com/questions/24145826/how-do-i-get-a-substring-of-a-string-in-rust)