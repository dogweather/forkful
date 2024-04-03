---
date: 2024-01-26 04:10:27.921171-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust\u652F\u6301\u5404\u79CD\u8C03\u8BD5\
  \u5668\uFF0C\u4F46\u5E38\u89C1\u7684\u6709GNU/Linux\u7684`gdb`\u6216macOS\u7684\
  `lldb`\u3002\u4F60\u4E5F\u53EF\u4EE5\u4F7F\u7528`rust-gdb`\u6216`rust-lldb`\uFF0C\
  \u8FD9\u4E9B\u662F\u5305\u88C5\u5668\uFF0C\u80FD\u7F8E\u5316\u6253\u5370Rust\u503C\
  \u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u4F8B\u5B50\uFF1A."
lastmod: '2024-03-13T22:44:47.525561-06:00'
model: gpt-4-0125-preview
summary: "Rust\u652F\u6301\u5404\u79CD\u8C03\u8BD5\u5668\uFF0C\u4F46\u5E38\u89C1\u7684\
  \u6709GNU/Linux\u7684`gdb`\u6216macOS\u7684`lldb`\u3002\u4F60\u4E5F\u53EF\u4EE5\u4F7F\
  \u7528`rust-gdb`\u6216`rust-lldb`\uFF0C\u8FD9\u4E9B\u662F\u5305\u88C5\u5668\uFF0C\
  \u80FD\u7F8E\u5316\u6253\u5370Rust\u503C\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u4F8B\
  \u5B50\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
