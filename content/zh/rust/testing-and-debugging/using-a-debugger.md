---
date: 2024-01-26 04:10:27.921171-07:00
description: "\u4F7F\u7528\u8C03\u8BD5\u5668\u5C31\u50CF\u7ED9\u81EA\u5DF1\u52A0\u4E0A\
  \u4E86X\u5C04\u7EBF\u89C6\u89C9\uFF0C\u80FD\u5077\u770B\u4EE3\u7801\u7684\u6267\u884C\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u627E\u51FA\u9519\u8BEF\
  \u3001\u7406\u89E3\u7A0B\u5E8F\u6D41\u7A0B\u3001\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\
  \u7801\u5E72\u5E72\u51C0\u51C0\u3002\u8FD9\u5C31\u50CF\u6709\u4E2A\u4F19\u4F34\u5728\
  \u4F60\u6454\u5012\u7684\u5730\u65B9\u6307\u7ED9\u4F60\u770B\u3002"
lastmod: '2024-03-13T22:44:47.525561-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u8C03\u8BD5\u5668\u5C31\u50CF\u7ED9\u81EA\u5DF1\u52A0\u4E0A\
  \u4E86X\u5C04\u7EBF\u89C6\u89C9\uFF0C\u80FD\u5077\u770B\u4EE3\u7801\u7684\u6267\u884C\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u627E\u51FA\u9519\u8BEF\
  \u3001\u7406\u89E3\u7A0B\u5E8F\u6D41\u7A0B\u3001\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\
  \u7801\u5E72\u5E72\u51C0\u51C0\u3002\u8FD9\u5C31\u50CF\u6709\u4E2A\u4F19\u4F34\u5728\
  \u4F60\u6454\u5012\u7684\u5730\u65B9\u6307\u7ED9\u4F60\u770B\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
