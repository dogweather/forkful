---
title:    "Rust: 提取子字符串"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么

提取子字符串是编程中常见的任务，在处理文本数据时特别有用。使用Rust编程语言，可以轻松地从一个字符串中提取部分文本，并将其用作其他操作的输入。如果您正在学习Rust，又想进一步提高您的编程技能，那么掌握提取子字符串的技巧是非常重要的。

## 如何进行

在Rust中，提取子字符串的操作是通过调用`.get()`或`.slice()`方法来实现的。让我们来看一个示例，假设我们有一个字符串`let string = "Hello, world!"`，我们想要提取其中的`Hello`。

```
Rust代码块
let string = "Hello, world!";
let substring = string.get(0..5);
```

在这个例子中，我们使用`.get()`方法来提取从索引0开始，长度为5的子字符串。`.get()`方法返回一个`Option<&str>`，因此我们可以使用`.unwrap()`方法来获取实际的子字符串。

```
Rust代码块
let string = "Hello, world!";
let substring = string.get(0..5).unwrap();
println!("{}", substring); // 输出 "Hello"
```

如果我们想要提取字符串的末尾部分，可以使用`.slice()`方法，该方法接受一个参数表示从哪个索引开始，直到字符串的结尾。

```
Rust代码块
let string = "Hello, world!";
let substring = string.slice(7..);
println!("{}", substring); // 输出 "world!"
```

可以看到，使用Rust提取子字符串是非常简单的。

## 深入了解

在Rust中，子字符串实际上是一个字符串的切片（slice）。这意味着它们只是原始字符串的一部分，而不是一个全新的字符串。这种设计使得在提取子字符串时，不会分配新的内存空间，从而提高了效率。

需要注意的是，在使用`.get()`和`.slice()`方法提取子字符串时，传递的索引参数是左闭右开区间。这意味着如果传递的索引值是`0..5`，将会提取从索引0到4的字符，不包括索引5。

## 参考文章

- [Official Rust Documentation on String Slices](https://doc.rust-lang.org/std/primitive.slice.html)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/string.html)
- [How to Extract a Substring from a String in Rust](https://www.codementor.io/@gowtham/dev-blog-how-to-extract-a-substring-from-a-string-in-rust-vuohr52i6)

## 参见

- [Rust字符串操作指南（Mandarin）](https://example.com/m/zh/strings-in-rust)
- [Rust常见编程任务指南（Mandarin）](https://example.com/m/zh/rust-programming-tasks)