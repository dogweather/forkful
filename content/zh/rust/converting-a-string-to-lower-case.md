---
title:                "Rust: 将字符串转换为小写"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：为什么有必要将字符串转换为小写？

Rust是一种现代的、快速的编程语言，它有许多实用的功能，其中之一就是可以轻松地将字符串转换为小写形式。以前，在其他语言中，可能需要编写一系列复杂的代码来实现这一功能。在Rust中，只需几行简单的代码就可以实现这一目的。

## 如何操作

要在Rust中将字符串转换为小写，我们需要使用字符串的 `to_lowercase()`方法。以下是一个简单的示例代码，展示如何使用这个方法：

```Rust
let my_string = "HELLO, MANDARIN!";
let lower_case_string = my_string.to_lowercase();
println!("{}", lower_case_string);
```

这段代码将会输出`hello, mandarin!`，所有的大写字母都被转换为小写。

## 深入探讨

除了 `to_lowercase()` 方法之外，Rust还提供了其他有用的方法来处理字符串，比如 `to_uppercase()` 方法来将字符串转换为大写形式，`trim()` 方法来去除字符串中的空格等。还有一些方法可以对字符串进行更复杂的操作，例如 `replace()` 方法用于替换字符串中的特定部分。

另外，值得一提的是，在Rust中，字符串是不可变的，也就是说，一旦创建好了就无法在原始字符串上做任何改变。因此，在进行字符串操作的过程中，Rust会返回一个新的字符串，而不会影响原始字符串。这就保证了程序的安全性和可靠性。

## 查看相关资料

如果你想要深入学习关于在Rust中处理字符串的更多信息，以下是一些相关的资源：

- [Rust官方文档](https://www.rust-lang.org/learn)
- [Rust字符串手册](https://doc.rust-lang.org/std/string/index.html)
- [Rust标准库中关于字符串处理的更多详细信息](https://doc.rust-lang.org/std/string/index.html)
- [Rust字符串相关的Crates](https://crates.io/keywords/string)

## 参考资料

[Convert string to lowercase in Rust](https://stackoverflow.com/questions/26324988/convert-string-to-lower-case)

[Rust Playground](https://play.rust-lang.org/)