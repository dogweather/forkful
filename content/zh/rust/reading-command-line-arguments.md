---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

命令行参数是你为程序提供的在命令行中输入的参数。程序员使用它们来定制程序的行为。

## 如何操作:

在Rust中，你可以使用标准库的`std::env::args()`来读取命令行参数。

```Rust
fn main() {
    for argument in std::env::args() {
        println!("{}", argument);
    }
}
```

如果你运行`cargo run arg1 arg2 arg3`，你将看到以下输出:

```Rust
target/debug/your_project
arg1
arg2
arg3
```

`std::env::args()`返回一个实现了`Iterator`接口的`Args`对象，这意味着你可以在返回值上调用`.next()`, `.nth(n)`等方法来取出你想要的参数。

## 深入探究

在Rust出现前，C和C++已经提供了一种通过命令行接口CLI来控制程序行为的方式，在Rust中也沿用了这一传统。虽然`std::env::args`是获取命令行参数的标准方式，但它并不提供参数解析功能，如带有标志的`-f`或`--file`。

对于更复杂的参数解析任务，Rust社区推荐的库是`clap`。它不仅可以自动地解析和验证参数，还可以生成帮助信息和错误信息。

```Rust
use clap::{Arg, App};

fn main() {
    let matches = App::new("My Super Program")
        .arg(Arg::with_name("file")
             .short("f")
             .long("file")
             .takes_value(true)
             .help("A cool file"))
        .get_matches();
    
    let file = matches.value_of("file").unwrap();
    println!("Using file: {}", file);
}
```

## 另请参阅

- [Rust官方关于`std::env`](https://doc.rust-lang.org/std/env/index.html)
- [`clap`文档](https://docs.rs/clap/2.33.3/clap/)
- [Crate.io上的命令行解析库](https://crates.io/categories/command-line-interface)