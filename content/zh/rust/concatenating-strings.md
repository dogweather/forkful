---
title:                "Rust: 字符串连接"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么要使用Rust连接字符串

连接字符串是在编程中常见的操作，它允许我们将多个字符串合并为一个。在Rust中，使用连接运算符`+`可以很容易地实现这一功能。除此之外，Rust还提供了多种方法来进行字符串连接，使得代码更加简洁高效。

## 如何进行字符串连接

在Rust中，我们可以使用连接运算符`+`来连接两个字符串，并将结果赋值给一个新的字符串变量。例如：

```Rust
let name = "John";
let message = "Hello, " + name;
println!("{}", message);
```

运行这段代码，会输出`Hello, John`。我们也可以使用`format!`宏来连接多个字符串，这样可以更加灵活地组合字符串。代码示例如下：

```Rust
let name = "John";
let age = 25;
let message = format!("My name is {} and I'm {} years old.", name, age);
println!("{}", message);
```

运行结果为`My name is John and I'm 25 years old.`

除了使用`+`运算符和`format!`宏，Rust还提供了`push_str`和`push`方法来进行字符串连接。`push_str`方法会将字符串附加到原有字符串的末尾，而`push`方法可以将一个字符附加到原有字符串的末尾。示例如下：

```Rust
let mut message = String::from("Hello, ");
message.push_str("John");
println!("{}", message);
```

运行结果为`Hello, John`。

## 深入了解字符串连接

在Rust中，字符串是UTF-8编码的，它们处于一个叫做`String`的类型中。`String`类型在Rust中是一个`vector`，因此使用连接运算符`+`或`push_str`方法来连接字符串时，会涉及到内存分配和移动数据的操作。这些操作可能会影响程序的性能，因此在处理大量数据时，建议使用`format!`宏或`push`方法来进行字符串连接，这样可以避免不必要的内存分配和数据移动。

## 参考链接

- [Rust官方文档：字符串连接](https://doc.rust-lang.org/std/string/struct.String.html#method.push_str)
- [Rust Cookbook: Concatenate Strings](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#concatenate-strings)
- [Rust编程语言教程：字符串和格式化输出](https://wiki.jikexueyuan.com/project/rust-primer/basics/string.html)