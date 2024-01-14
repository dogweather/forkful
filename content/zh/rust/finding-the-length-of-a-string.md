---
title:    "Rust: 查找字符串的长度"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么

在日常的编程中，我们经常会遇到需要知道字符串长度的情况。无论是验证用户输入的合法性，还是进行字符串的截取和处理，都需要我们知道字符串的长度。而Rust语言提供了简单而高效的方法来获取字符串的长度，让我们一起来看看吧！

## 如何实现

要获取字符串的长度，我们可以使用标准库提供的String类型的len()方法。只需要将字符串变量名后面加上.len()即可。下面是一个简单的示例代码：

```Rust
let str = String::from("Rust编程是一门非常有趣的语言");
let len = str.len();
println!("字符串的长度为：{}", len);
```

输出结果为：字符串的长度为：16

从输出结果可以看出，通过len()方法我们得到了字符串的长度，简洁高效。不仅如此，我们还可以通过bytes()方法获取字符串的字节长度，这在处理unicode字符时非常有用。

```Rust
let str = String::from("Rust编程是一门非常有趣的语言");
let byte_len = str.bytes().len();
println!("字符串的字节长度为：{}", byte_len);
```

输出结果为：字符串的字节长度为：28

## 深入探究

在Rust中，字符串实际上是一个动态分配的字节数组。而len()方法返回的就是这个字节数组的长度，因此它既可以用于获取字符串的长度，也可以用于获取字节长度。另外，Rust还提供了capacity()方法来获取字符串的容量，即分配给字符串的字节数组的总长度。这在使用字符串做字符串拼接等操作时，能够帮助我们提前分配足够的空间，从而避免不必要的内存分配和拷贝，提高性能。

## 参考资料

[The Rust Standard Library Documentation](https://doc.rust-lang.org/std/string/struct.String.html)

[Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)

## 参见

见其他文章了解更多有关Rust的知识：

[Rust学习路线：从入门到精通](https://www.jianshu.com/p/efea00381632)

[使用Rust开发Web应用：实现一个简单的TodoList](https://www.jianshu.com/p/0bbaa1b20657)

[Rust和Python比较：两种不同的编程风格](https://www.jianshu.com/p/fdfc9cb70ac2)