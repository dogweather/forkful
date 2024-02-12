---
title:                "使用调试器"
aliases: - /zh/rust/using-a-debugger.md
date:                  2024-01-26T04:10:27.921171-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用调试器就像给自己加上了X射线视觉，能偷看代码的执行。程序员这样做是为了找出错误、理解程序流程、确保他们的代码干干净净。这就像有个伙伴在你摔倒的地方指给你看。

## 如何操作：

Rust支持各种调试器，但常见的有GNU/Linux的`gdb`或macOS的`lldb`。你也可以使用`rust-gdb`或`rust-lldb`，这些是包装器，能美化打印Rust值。以下是一个例子：

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

要调试这段代码，请带着调试信息编译：

```shell
$ rustc -g counter.rs
```

然后在`rust-gdb`中运行它：

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## 深入探讨

调试自打穿孔卡片时代就有了，其发展简直是天赐之物。由于Rust的系统级特性，Rust提供了自己的工具，集成了GDB和LLDB。

除了GDB和LLDB，调试Rust代码的替代方法包括使用带有内置调试器的集成开发环境（IDE），有些人觉得这更直观。流行的IDE包括带有Rust插件的CLion或带有Rust扩展的Visual Studio Code。

至于实现，Rust生成这些调试器能理解的调试符号，对于逐步执行代码、设置断点、检查变量而不丧失理智至关重要。

## 另请参阅

- Rust Book上的调试：https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example上关于错误和调试的解读：https://doc.rust-lang.org/rust-by-example/error.html
- 支持VS Code的Rust扩展的Rust语言服务（RLS）：https://github.com/rust-lang/rls
- 使用Visual Studio Code调试Rust：https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
