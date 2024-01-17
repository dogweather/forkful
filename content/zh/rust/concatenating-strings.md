---
title:                "连接字符串"
html_title:           "Rust: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串连接是指将多个字符串合并成一个长字符串的过程。程序员通常执行这个操作是为了将多个短字符串组成一个更长的字符串，以便在打印输出或存储数据时更方便。

## 如何：
```Rust
// 使用 `format!` 宏进行字符串连接
let name = "Alice";
let message = format!("Hello, {}!", name);
println!("{}", message);

// 使用 `+` 运算符进行字符串连接
let str1 = "Hello,";
let str2 = "World!";
let message = str1 + " " + str2;
println!("{}", message);
```

**输出：**
```
Hello, Alice!
Hello, World!
```

## 深入了解：
(1) 在早期的编程语言中，字符串连接是一种昂贵的操作，因为它会频繁地分配内存和复制字符串。但是在现代编程语言中，如Rust，这种操作已经被优化，因此程序员可以放心地使用字符串连接来提高代码的可读性。
(2) 其他替代的方法包括使用模板字符串或使用流式操作符，如Haskell中的`<|`操作符。
(3) 在Rust中，字符串连接的实现是通过`format!`宏和`+`运算符来实现的。`format!`宏执行的是字符串格式化操作，而`+`运算符则通过使用`Deref` trait来调用`String`类型的`add()`方法来实现字符串的连接。

## 参考链接：
- [Rust官方文档：`format!`宏](https://doc.rust-lang.org/std/macro.format.html)
- [Rust官方文档：`String`类型](https://doc.rust-lang.org/std/string/struct.String.html)
- [知乎：Haskell中的`<|`操作符](https://www.zhihu.com/question/33747467)