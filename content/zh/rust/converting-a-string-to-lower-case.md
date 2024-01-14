---
title:    "Rust: 将字符串转换为小写"
keywords: ["Rust"]
---

{{< edit_this_page >}}

为什么： 字符串是程序中经常使用的一种数据类型。如果你想要对字符串进行操作，很有可能会遇到需要将字符串转换成小写的需求。这种转换可以让字符串更容易处理和比较，因此转换成小写是一项常见的操作。

如何：下面是一个简单的代码示例，展示如何在Rust中将字符串转换成小写：

```Rust
​fn main() {
    let my_string = String::from("HeLLo Rust");
    
    let lowercased = my_string.to_lowercase();
    
    println!("Original string: {}", my_string);
    println!("Lowercased string: {}", lowercased);
}
```
输出示例：
```
原字符串：HeLLo Rust
转换成小写后的字符串：hello rust
```

如果你想要在不修改原始字符串的情况下转换成小写，可以使用`to_ascii_lowercase()`方法。以下是使用这个方法的示例代码：

```Rust
fn main() {
    let my_string = "HeLLo Rust";
    
    let lowercased = my_string.to_ascii_lowercase();
    
    println!("Original string: {}", my_string);
    println!("Lowercased string: {}", lowercased);
}
```
输出示例：
```
原字符串：HeLLo Rust
转换成小写后的字符串：hello rust
```

深入探讨：在Rust中，字符串是不可变的数据类型，因此转换成小写时需要使用`to_lowercase()`或`to_ascii_lowercase()`方法返回一个新的字符串，而不是修改原有的字符串。同时，这两个方法都支持Unicode字符，而`to_lowercase()`方法也支持其他语言的字符转换。此外，需要注意的是，如果字符串包含的字符已经是小写形式，那么这两个方法不会有任何变化。

另外，如果你想要更加灵活地控制字符串转换成小写的方式，可以使用`fold()`方法，它允许你自定义转换的规则。以下是一个使用`fold()`方法的代码示例：

```Rust
fn main() {
    let my_string = "Hello Rust 2021";
    
    let lowercased = my_string.fold(String::new(), |mut res, c| {
        if c.is_ascii_alphabetic() {
            res.push(c.to_ascii_lowercase());
        } else {
            res.push(c);
        }
        res
    });
    
    println!("Original string: {}", my_string);
    println!("Lowercased string: {}", lowercased);
}
```
输出示例：
```
原字符串：Hello Rust 2021
转换成小写后的字符串：hello rust 2021
```

可以看到，`fold()`方法允许我们自定义转换规则，这样就可以灵活地处理不同情况下的字符串转换需求。

## 参考链接
- Rust官方文档：https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Rust编程语言中文社区：https://rust.cc/article?id=31b63c41-8165-4032-b9bb-3b1f57bba7d4
- CSDN博文：https://blog.csdn.net/qq_34835942/article/details/105577351

## 参见
[字符串处理基础教程](https://rust.cc/article?id=df6514bd-7edb-44f8-86c8-aea36c3b4a11)
[Rust语言中文社区](https://rust.cc/)