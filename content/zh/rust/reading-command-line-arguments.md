---
title:    "Rust: 读取命令行参数"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么
在编程世界中，命令行参数是一个不可或缺的组成部分。它们允许我们以不同的方式从命令行运行程序，并提供程序所需的信息。Rust编程语言提供了一种简单、清晰和高效的方法来读取命令行参数，并且本文将向你展示如何做到这一点。

## 如何操作
首先，让我们来看一个简单的代码示例，来读取并打印出命令行参数：
```rust
use std::env;
fn main() {
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    // 遍历并打印出每个参数
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```
假设我们的程序叫做"args"，那么从命令行运行它，并传入参数"hello world"，将输出：
```
args
hello
world
```
这很简单，我们使用了Rust标准库中的环境模块来获取命令行参数，然后使用迭代器来遍历并打印出每个参数。但是，通常我们可能只需要获取特定位置的参数，比如第一个和第二个参数。让我们来改进一下我们的代码：
```rust
use std::env;
fn main() {
    // 获取第一个和第二个参数
    let first_arg = env::args().nth(1).unwrap();
    let second_arg = env::args().nth(2).unwrap();
    // 打印出它们
    println!("First argument: {}", first_arg);
    println!("Second argument: {}", second_arg);
}
```
现在，我们只获取了第一个和第二个参数，并打印出它们。值得注意的是，为了获取特定位置的参数，我们使用了迭代器的`nth()`方法，并使用`unwrap()`来解包可能的空值。如果想要获取其他位置的参数，只需要调用相应的索引即可。

## 深入探究
除了获取特定位置的参数，Rust还提供了一种更灵活的方法来读取命令行参数，即使用`getopts`库。这个库允许我们定义命令行选项，将参数以键值对的形式保存，并提供帮助信息和错误处理。让我们来看一个例子：
```rust
extern crate getopts; // 添加依赖

use getopts::Options;
use std::env;
fn main() {
    // 定义程序的帮助信息
    let program = "args";
    let brief = format!("Usage: {} [options]", program);
    
    // 定义命令行选项
    let mut opts = Options::new();
    opts.optflag("h", "help", "Display this help message");
    opts.optflag("v", "verbose", "Enable verbosity");
    
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    let matches = opts.parse(&args[1..]).unwrap();
    
    // 判断是否有帮助选项，并打印出帮助信息
    if matches.opt_present("h") {
        println!("{}", opts.usage(&brief));
        return;
    }
    
    // 判断是否启用了 verbosity
    if matches.opt_present("v") {
        println!("Enabled verbosity");
    }
    
    // 打印出其他参数
    for arg in matches.free {
        println!("{}", arg);
    }
}
```
现在，我们的程序可以接受两个命令行选项，即`-h`和`-v`，并打印出对应的信息。如果想要了解更多关于`getopts`库的用法，可以参考它的[文档](https://docs.rs/getopts/)。

# 参考链接
- [Rust官方文档](https://www.rust-lang.org/zh-CN/)
- [Rust标准库文档](https://doc.r