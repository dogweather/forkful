---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串连接是将两个或多个字符串连成一个的过程。程序员这样做是为了将多个字符串值合并成一个新的长字符串。

## 如何实现：

在Rust编程语言中，我们有许多方法可以连接字符串。其中的一些例子包括使用`+`运算符和`format!`宏。下面是一些使用这些功能进行字符串连接的例子：

```Rust
let a = "Hello, ".to_string();
let b = "World!";
let c = a + b;

println!("{}", c);
```

这将输出：`Hello, World!`

或者您可以使用`format!`宏更好地管理不同类型的数据。

```Rust
let a = "Hello, ";
let b = "World!";
let c = format!("{}{}", a, b);

println!("{}", c);
```

这也将输出：`Hello, World!`

## Deep Dive：

在计算机科学的早期，因为硬件的限制，字符串的操作被看作是一项非常昂贵的操作。但随着科技的发展，硬件已经足够强大，足以在常规应用程序中处理大量字符串操作。

在Rust中，与C和C++等旧语言不同，内存管理在很大程度上被自动化，因此减轻了程序员的负担。在进行字符串连接时，Rust的`String`数据类型会自动为新字符串分配必要的内存空间。

您也可以考虑使用`join()`方法连接字符串数组：

```Rust
let strs = ["Hello, ", "World!"];
let c = strs.join("");

println!("{}", c);
```

这也会输出：`Hello, World!`

## 参考看看：

1. Rust by Example: https://doc.rust-lang.org/rust-by-example/std/str.html
2. The Rust Programming Language Book: https://doc.rust-lang.org/book/ch08-02-strings.html
3. Rust之字符串处理（翻译）: https://blog.csdn.net/weixin_30391145/article/details/98654107