---
title:                "写入标准错误"
html_title:           "Rust: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#为什么

为什么要将信息写入标准错误（standard error）？有时候我们的程序可能会出现一些错误，当我们想要知道这些错误是什么时，就可以通过将信息写入标准错误来进行调试和排查问题。

#如何操作

要将信息写入标准错误，我们可以使用标准库中的 `eprintln!` 宏。这个宏可以将信息以错误的形式打印出来，并且会自动将信息写入标准错误。

```Rust
let number = 42;

eprintln!("错误：数字 {} 超出范围", number);
```

输出结果：

```
错误：数字 42 超出范围
```

#深度了解

标准错误是处理错误信息的重要工具，它可以让我们快速定位错误并进行调试。除了使用 `eprintln!` 宏外，我们还可以使用 `writeln!` 宏来将信息写入其他的流，比如标准输出或者文件。

#相关阅读

- [Rust 标准库文档](https://doc.rust-lang.org/std/)
- [深入理解标准错误](https://blog.csdn.net/u011436429/article/details/79855920)
- [Rust 语言入门教程](https://rustlang-cn.org/office/rust/book/getting-started.html)

#参考链接

- [《Rust 程序设计语言》官方中文版](https://rustlang-cn.org/office/rust/book/title-page.html)
- [Rust 语言编程指南](https://www.runoob.com/rust/rust-basic-syntax.html)