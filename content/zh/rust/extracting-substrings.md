---
title:                "Rust: 提取子字符串"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取字符串是编程中常见的操作，它可以帮助我们在处理文本数据时更加灵活和高效。使用Rust语言可以使提取子字符串的过程更加安全和可靠。

## 如何提取子字符串

要提取子字符串，我们可以使用Rust标准库中的```get```方法，它接受两个参数：起始位置和结束位置。下面是一个简单的例子:

```Rust
let s = "Hello, world!";
let substring = s.get(0..5); // 提取从0到5（不包括5）的子字符串
println!("{}", substring); // 输出 Hello
```

我们还可以使用```get_mut```方法来提取可变的子字符串，以便修改它。此外，我们也可以使用```slice```方法来提取超出边界的子字符串。

## 深入了解子字符串提取

在Rust中，字符串是不可变的，并且字符串的切片也是不可变的。当我们提取子字符串时，实际上创建了原字符串的一个引用，这样就可以避免不必要的内存分配和拷贝。此外，Rust还提供了```String```类型来表示可变的字符串，我们可以使用它来构建和修改字符串，然后再进行提取子字符串操作。

## 参考资料

- [Rust官方文档：字符串](https://doc.rust-lang.org/std/string/)
- [Rust官方文档：字符串切片](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
- [Rust Cookbook：提取子字符串](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#extract-a-substring)