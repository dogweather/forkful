---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？

将字符串转换为小写，本质上是将字符串内的所有大写字母修改为其对应的小写形式。程序员之所以需要做这一点，主要是用于在处理用户输入或数据处理时消除大小写带来的影响，如搜索算法和用户数据的比较。

## 怎么做：

在Rust中，我们通过使用 `to_lowercase()` 函数将字符串转换为小写。

我们来看一个例子：

```Rust
fn main() {
    let s = "Hello, Rust World!";
    let s_lower = s.to_lowercase();
    println!("{}", s_lower);
}
```
输出结果为：

```Rust
hello, rust world!
```

## 深入了解

早年的计算机编程通常不涉及大小写转换的问题，因为那时的计算机系统主要处理的是数字和简单字符。然而，随着个人计算设备的出现和使用的普及，处理文本信息的需求增加了，这也就包括我们现在讨论的大小写转换问题。

在Rust中，字符串默认是不可改变的，使用 `to_lowercase()` 后将返回一个新的小写字符串。 如果你觉得 `to_lowercase()` 功能不足以满足你的需求，可以使用正则表达式库 `regex` 为处理更具挑战性的字符串大小写转换问题。

在内部实现上，`to_lowercase()` 函数会遍历原字符串的每个字符，并将其替换为小写版本。如果字符本身就是小写或者不区分大小写，那么就原样返回。

## 另请参阅：

Rust官方文档关于Strings对大小写处理的更多信息：[https://doc.rust-lang.org/std/string/struct.String.html](https://doc.rust-lang.org/std/string/struct.String.html)

关于正则表达式的用法和教程，可以查看：[https://doc.rust-lang.org/regex/regex/index.html](https://doc.rust-lang.org/regex/regex/index.html)