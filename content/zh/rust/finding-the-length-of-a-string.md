---
title:    "Rust: 计算字符串的长度"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

Rust是一种快速，可靠且安全的系统编程语言，它支持高性能和并发编程。字符串是编程中常用的数据类型，因此学习如何在Rust中计算字符串的长度是非常有用的技能。通过这篇博文，你将学习如何用Rust编写一个简单的程序来计算字符串的长度。

## 如何做

要计算一个字符串的长度，在Rust中我们可以使用`len()`函数。下面是一个简单的例子，演示了如何使用`len()`函数计算字符串的长度并打印出来：

```Rust
fn main() {
    let my_string = "Hello Rust!";
    let length = my_string.len();
    println!("The length of the string is {}", length);
}
```

这段代码创建了一个字符串变量`my_string`，然后使用`len()`函数计算这个字符串的长度并赋值给变量`length`。最后，使用`println!`宏来打印出字符串的长度。你可以在终端运行这段代码，你会看到输出结果为：

```
The length of the string is 11
```

这是因为上面的字符串有11个字符，包括空格和标点符号。

## 深入探讨

在Rust中，`len()`函数计算的是字符串的字节长度，而不是字符数。这是因为Rust使用UTF-8编码来表示字符串，这种编码方式会给每个字符分配不同数量的字节。因此，一个字符串的字节长度并不总是等于它的字符数。例如，一个包含汉字的字符串，在Rust中的字节长度可能会比它的字符数多。

还有一个需要注意的地方是，`len()`函数返回的是一个`usize`类型的值，它表示一个指针在当前系统中所占据的字节数。这意味着，如果你的系统是64位的，那么`usize`类型的大小就是8个字节，所以它可以存储的最大字符串长度也只能是`usize`的最大值。

如果你想要以字符数的形式获取字符串的长度，可以使用`chars()`方法来计算。这个方法会返回一个迭代器，你可以使用`.count()`来计算迭代器的长度，也就是字符数。下面是一个例子：

```Rust
fn main() {
    let my_string = "Hello 你好";
    let length = my_string.chars().count();
    println!("The length of the string is {}", length);
}
```

这段代码输出的结果是：

```
The length of the string is 7
```

这是因为这个字符串包含了7个字符，包括空格和汉字。

## 参考资料

- [The Rust Programming Language](https://www.rust-lang.org/zh-CN/)
- [Rust官方文档](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Rust String类型](https://doc.rust-lang.org/std/string/struct.String.html)
- [UTF-8编码](https://zh.wikipedia.org/wiki/UTF-8)
- [Rust字符串的字节长度与字符数](https://www.tutorialspoint.com/rust/rust_string_length.htm)

## 参见

- [Rust中的字符串处理](https://rustlang-cn.org/office/rust/advanced/strings-processing.html)
- [Rust字符串方法](https://doc.rust-lang.org/std/string/struct.String.html#methods)
- [Rust字符串切割](https://rustbyexample.com/std/str/split.html)