---
title:                "Rust: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##为什么

编程时，有时候我们可能会遇到一些错误或者异常情况。当出现这些情况时，我们需要将它们记录下来以便后续排查。而将这些信息输出到标准错误流（standard error）是一种常见的做法。通过这篇文章，我们将详细介绍如何使用Rust语言来写入标准错误。

##如何进行

在Rust中，我们可以使用标准库中的`eprintln!`宏来实现向标准错误流输出信息。
```
Rust
fn main() {
    eprintln!("This is an error message!");
}
```
这样就可以在程序出错时，向标准错误流输出一条错误信息。除了简单的字符串，我们也可以传入变量来输出更加详细的信息。
```
Rust
fn main() {
    let value = 10;
    eprintln!("The value is {}", value);
}
```
输出结果会是：`The value is 10`。

##深入了解

在Rust中，标准错误流通常用来输出一些重要的信息，比如错误提示、警告等。在编写更复杂的程序时，我们可能需要自定义错误类型来更精确地指定错误信息。这时，我们可以使用`eprintln!`宏来输出自定义的错误类型。
```
Rust
use std::error::Error;
use std::fmt;
#[derive(Debug)]
struct CustomError;
impl Error for CustomError {}
impl fmt::Display for CustomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "This is a custom error!")
    }
}
fn main() -> Result<(), Box<dyn Error>> {
    Err(Box::new(CustomError))
}
```
在这个例子中，我们定义了一个自定义错误类型`CustomError`，并在`main`函数中返回了一个包含该错误类型的`Result`。当程序运行时，我们会在标准错误流中看到输出信息`This is a custom error!`。

##参考链接

- [Rust标准库文档](https://rust-lang.github.io/stdlib/doc/std/io/index.html#standard-error-stream)
- [Rust官方教程](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Rust编程语言](https://www.rust-lang.org/)


##参见

- [将错误信息输出到标准输出流的方法](https://example.com)（英文）