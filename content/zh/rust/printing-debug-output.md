---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
打印调试输出就是将程序处理的信息打印出来，以便于程序员观察和分析程序的运行情况。程序员之所以需要它，是因为这样可以帮助他们更轻松地排查和修复代码中的错误。

## 如何操作：
Rust中，使用`println!`宏可以输出信息。如果你想输出调试信息，可以使用 `{:?}` 格式规范。看看下面的示例：

```Rust
let x = 5;
println!("x 的值是: {:?}", x);
```

输出如下：

```Rust
x 的值是: 5
```

## 深入探讨:
在早期的编程中，打印调试输出是追踪问题的主要方式。现在，我们有专门的调试工具，但当调试工具使用起来很复杂或者不能获得运行时的完整视图时，打印依然是一种有效的解决方案。
   
打印调试输出的另一种方式是使用 `debug!` 宏,这需要在项目中包含 `log` 库。你也可以自定义打印方法，但这可能需要更多的工作。
   
实现细节方面，`println!` 宏实际上是`print!` 宏与 `\n` 换行的结合。`{:?}` 格式规范调用类型的`std::fmt::Debug`实现，如果类型没有实现`Debug`，代码则无法编译。

## 参考资料：
[Rust 的 println! 参考](https://doc.rust-lang.org/stable/std/macro.println.html)
[Rust 的 print! 参考](https://doc.rust-lang.org/stable/std/macro.print.html)
[Rust 的 格式化输出参考](https://doc.rust-lang.org/stable/std/fmt/)