---
title:    "Rust: 向标准错误输出编写 (Xiàng biāozhǔn cuòwù shūchū biānxíe)"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

写作标准错误信息可能是一项令人生畏的任务，但它却是一个重要的编程技能。当程序出现错误时，标准错误信息可以帮助我们快速定位问题并进行调试。在Rust编程中，使用标准错误信息可以让我们更加有效地发现和修复错误，提高代码质量，从而使我们的程序更加健壮可靠。

## 如何使用

在Rust中，我们可以使用`std::io::stderr`模块来向标准错误流输出信息。让我们来看一个简单的示例：

```Rust
use std::io::Write;

fn main() {
    let error_msg = "This is an error message written to standard error.";
    let mut stderr = std::io::stderr();

    writeln!(stderr, "{}", error_msg).expect("Failed to write to stderr");
}
```

在这个示例中，我们首先导入`std::io::Write`模块，它提供了写入流的功能。然后，我们定义一个字符串变量`error_msg`，它是我们想要输出的错误信息。接着，我们通过`std::io::stderr`函数创建一个标准错误流的实例，并使用`writeln!`宏来输出错误信息到流中。最后，我们使用`expect`方法来检查是否出现了错误。

运行上面的代码，你会在控制台看到以下输出：

```sh
This is an error message written to standard error.
```

正如你所见，错误信息被输出到了标准错误流中。除了使用`writeln!`宏，我们还可以使用`write!`宏来向标准错误流输出信息，不同之处在于`writeln!`会在字符串的末尾自动添加换行符。

## 深入讨论

在Rust中，我们可以使用`std::io::stderr`函数来获取标准错误流的实例，它返回一个`Stderr`结构体。这个结构体实现了`Write` trait，使得我们可以像操作其他写入流一样来操作标准错误流。

除了使用`writeln!`和`write!`宏，我们还可以使用`write_all`和`flush`方法来向标准错误流输出信息。`write_all`方法可以一次性写入一个完整的字符串，而`flush`方法则可以强制刷新流，将缓冲区中的数据输出到终端。

另外，我们还可以使用`fmt::Display`和`fmt::Debug` trait来格式化输出信息。这两个trait可以帮助我们将变量转换为可打印的字符串，方便我们输出更复杂的错误信息。

## 参考链接

- [Rust标准库文档](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [使用标准错误信息调试Rust程序](https://doc.rust-lang.org/book/ch09-00-error-handling.html#to-panic-or-not-to-panic)
- [标准库中的写入流功能](https://doc.rust-lang.org/std/io/trait.Write.html)