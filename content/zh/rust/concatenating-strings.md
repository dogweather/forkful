---
title:    "Rust: 字符串连结"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么

字符串拼接是编程中常见的操作，它的主要作用是将多个字符串连接在一起，形成一个新的字符串。在Rust语言中，使用`+`运算符可以实现字符串的拼接。有时候我们需要将多个变量或者文本内容拼接成一个完整的字符串，这时就需要使用字符串拼接的技巧了。

## 如何实现

下面是一个简单的Rust代码示例，演示了如何使用`+`运算符进行字符串拼接：

```Rust
let hello = "你好";
let name = "小明";
let message = hello + name; // 将hello和name拼接成一个新字符串
println!("{}", message); // 输出：你好小明
```

通过上面的代码，可以发现我们只需要使用`+`运算符就能将字符串进行拼接。需要注意的是，两个字符串的类型必须相同，否则无法进行拼接。

## 深入了解

在Rust中，字符串属于不可变类型，意味着一旦声明就不能修改。因此，在字符串拼接过程中，并不是修改了原有字符串，而是创建了一个全新的字符串。这也是Rust语言的一大特点，它保证了数据的安全性和可变性。

此外，Rust还提供了更多的方法来进行字符串拼接，比如使用`format!`宏，它可以更灵活地插入变量，实现更复杂的字符串拼接需求。

## 参考链接

- [Rust官方文档-字符串拼接](https://doc.rust-lang.org/std/string/struct.String.html#impl-Add%3CT%3E)
- [Rust编程从入门到实践-字符串拼接](https://kaisery.github.io/trpl-zh-cn/ch08-02-strings.html#%E5%88%A9%E7%94%A8%E6%96%87%E4%BB%B6%E7%9A%84%E9%83%A8%E5%88%86%E5%8F%91%E5%B8%83%E3%80%81%E5%AD%97%E7%AC%A6%E4%B8%B2%E8%BF%9E%E6%8E%A5%E6%8E%A5%E5%8F%A3)
- [Visual Rust-深入理解Rust字符串](https://visualrust.net/resource/advanced/string-scoping-internal/)

## 参见

- [Rust官方文档](https://www.rust-lang.org/)
- [Rust中文社区](https://rust.cc/)
- [Rust入门教程](https://learnku.com/docs/rust-lang/2021)