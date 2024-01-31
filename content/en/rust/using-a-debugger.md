---
title:                "Using a debugger"
date:                  2024-01-25T20:50:25.605338-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?

Using a debugger is like giving yourself x-ray vision to peek into your code's execution. Programmers do it to spot bugs, understand program flow, and ensure their code is as clean as a whistle. It’s like having a buddy that points out exactly where you've tripped up.

## How to:

Rust supports various debuggers, but a common one is `gdb` for GNU/Linux or `lldb` for macOS. You might also use `rust-gdb` or `rust-lldb` which are wrappers that pretty-print Rust values. Here’s a glimpse:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

To debug this, compile with debug info:

```shell
$ rustc -g counter.rs
```

Then run it in `rust-gdb`:

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

## Deep Dive

Debugging's been around since *ye olde times* of punch cards, and its evolution has been a godsend. Rust provides its own tooling with integrations for GDB and LLDB due to the language’s system-level nature.

Alternatives for debugging Rust code include using integrated development environments (IDEs) with their built-in debuggers, which some find more intuitive. Popular ones include CLion with the Rust plugin or Visual Studio Code with the Rust extension.

As for implementation, Rust generates debug symbols that these debuggers understand, which is vital for stepping through the code, setting breakpoints, and inspecting variables without losing your marbles.

## See Also

- The Rust Book on Debugging: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example’s take on Errors and Debugging: https://doc.rust-lang.org/rust-by-example/error.html
- The Rust Language Server (RLS) which powers VS Code's Rust extension: https://github.com/rust-lang/rls
- Debugging Rust with Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
