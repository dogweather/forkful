---
title:                "打印调试输出"
html_title:           "Rust: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么要输出调试信息

无论你是正在开发一款全新的软件，还是在维护现有的代码，输出调试信息都是一个非常有用的工具。它可以帮助你跟踪代码的执行过程，发现潜在的问题，从而提高代码的质量。

## 如何输出调试信息

要输出调试信息，你首先需要在代码中添加一条语句，使用`println!`宏来打印你想要检查的信息。下面是一个简单的例子：

```Rust
println!("Hello, world!");
```

这条语句会将`Hello, world!`这个字符串打印到控制台上。

如果你想打印变量的值，可以在`println!`宏中使用占位符`{}`来代替变量。例如：

```Rust
let num = 42;
println!("The answer is {}", num);
```

这样就可以将变量`num`的值打印出来。

除了使用占位符，`println!`宏还支持格式化输出，比如指定小数点后的位数、十六进制格式等。更多关于格式化输出的内容，可以参考[Rust官方文档](https://doc.rust-lang.org/std/fmt/index.html#printing)。

## 深入了解打印调试信息

除了`println!`宏之外，Rust还提供了其他更多的宏用于打印调试信息。其中，最常用的是`dbg!`宏，它会在打印信息的同时，也会将变量的值打印出来，非常适合快速查看变量的内容。例如：

```Rust
let name = "John";
let age = 30;
dbg!(name, age);
```

这条语句会打印出两个变量的值：

```
[src/main.rs:3] name = "John"
[src/main.rs:4] age = 30
```

除了宏，Rust还提供了`eprint!`和`eprintln!`来将信息打印到标准错误流中，而不是标准输出。这在处理一些错误或异常时非常有用。

总的来说，打印调试信息是Rust中一个非常重要的工具，它可以帮助你快速定位代码中的问题，提高开发效率。

# 参考资料

- [Rust语言参考手册](https://doc.rust-lang.org/reference/index.html)
- [Rust标准库文档](https://doc.rust-lang.org/std/index.html)
- [Rust官方在线学习资源](https://www.rust-lang.org/learn)

# 参见

- [Rust语言的其他功能介绍](https://rustlang-cn.org/learn.html)
- [使用Rust编写高性能的Web应用](https://zhuanlan.zhihu.com/p/146386796)