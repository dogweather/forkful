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

## 为什么

当我们处理字符串时，有时我们只需要其中的一部分，而不是完整的字符串。这就是提取子字符串的作用：它允许我们从一个较大的字符串中提取出我们需要的部分。

## 如何进行

使用Rust内置的`slice`功能非常简单，只需要使用`&`符号来指定我们想要提取的起始和结束位置。让我们看一个例子：

```Rust
let full_string = "Hello World";
let substring = &full_string[0..5]; // 指定起始和结束位置
println!("{}", substring); // 输出 "Hello"
```

我们使用`&full_string[0..5]`来指定我们想要提取的子字符串的起始和结束位置。注意，结束位置是排除在外的，所以我们实际上提取的是0到4的字符。因此，输出为"Hello"。

如果我们想要提取的子字符串从起始位置开始，我们可以简写为：

```Rust
let full_string = "Hello World";
let substring = &full_string[..5]; // 省略起始位置
println!("{}", substring); // 输出 "Hello"
```

同样，如果我们想提取从某个位置到结尾的剩余部分，我们可以省略结束位置：

```Rust
let full_string = "Hello World";
let substring = &full_string[6..]; // 省略结束位置
println!("{}", substring); // 输出 "World"
```

我们还可以使用负数的索引来指定相对于字符串末尾的位置，例如：

```Rust
let full_string = "Hello World";
let substring = &full_string[-5..]; // 从倒数第五个字符开始提取
println!("{}", substring); // 输出 "World"
```

另外，我们也可以使用`chars`方法来指定字符的位置，而不是使用索引。这对于处理Unicode字符非常有用。让我们来看一个例子：

```Rust
let full_string = "你好，世界";
let substring = &full_string.chars().skip(3).take(2).collect::<String>(); // 从第三个字符开始提取两个字符
println!("{}", substring); // 输出 "世界"
```

在上面的代码中，我们使用`chars`方法来将字符串转换为字符迭代器，然后使用`skip`和`take`方法来指定我们想要提取的字符的位置。最后，我们将提取出的字符放入一个字符串中并打印出来。

## 深入探讨

除了提取子字符串外，我们还可以进行更复杂的操作，例如比较两个子字符串是否相等：

```Rust
let first = "Hello";
let second = "hello";
if first.eq_ignore_ascii_case(second) { // 忽略大小写比较
    println!("The two substrings are equal!");
} else {
    println!("The two substrings are not equal.");
}
```

另外，Rust也提供了一些简便的方法来提取子字符串，例如`split`和`matches`。这些方法在处理文本数据时非常有用，让我们以后的文章中深入探讨它们。

## 查看更多

- [Rust文档 - 字符串处理](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust by Example - 字符串操作](https://doc.rust-lang.org/rust-by-example/variable_bindings/string.html)
- [Rust Cookbook - 子字符串](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#extract-a-substring-of-bytes-from-a-string)