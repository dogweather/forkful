---
title:                "提取子字符串"
html_title:           "Rust: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

What & Why: 提取子串是指从一个字符串中获取一个子集，通常是通过指定起始和结束位置来实现。程序员们在提取子串时通常是为了处理文本数据或者进行字符串的操作。

如何: Rust提供了方便易用的方法来提取子串，使用 ```&``` 符号和范围操作符 ```..``` 来指定起始和结束位置即可。代码示例如下：
```Rust
let sentence = "这是一段文字";
let substring = &sentence[0..2]; // 从位置0到2提取子串
println!("提取的子串为： {}", substring);
```
输出结果：提取的子串为：这是

深入探讨: 提取子串一直是字符串处理的基本操作，然而在不同的编程语言中，所用的方法可能会有所不同。在早期，C语言中使用的是 ```substr``` 函数，而在Rust中则使用了切片的概念来提取子串。除了切片之外，还可以通过正则表达式来提取特定的子串，但是使用正则表达式会更加复杂一些。

参考资料: 想要了解更多关于提取子串的知识，可以参考Rust官方文档的[String Slices](https://doc.rust-lang.org/std/str/struct.String.html#method.slice.html) 和[Regular Expressions](https://doc.rust-lang.org/std/str/struct.String.html#method.matches.html)部分。