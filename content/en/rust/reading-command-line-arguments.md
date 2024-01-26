---
title:                "Reading command line arguments"
date:                  2024-01-20T17:57:02.616433-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Rust lets programs take user input on launch. It's key for custom behavior without a GUI.

## How to:

Here's the simplest way to grab arguments:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

Run it with `cargo run arg1 arg2`. You'll see:

```
["path/to/executable", "arg1", "arg2"]
```

A tidier option with iterators:

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

Now try `cargo run cool stuff`:

```
cool
stuff
```

## Deep Dive

Historically, command line arguments are a throwback to the days when GUIs weren't widespread. Now, they're great for scripts, servers, or tools.

Rust's `std::env::args` uses an iterator, which is memory efficient and lazy. It handles Unicode too. There's also `args_os` for raw OS strings.

For complex parsing, crates like `clap` or `structopt` come in handy. They parse flags, options, and subcommands.

## See Also

- [The Rust `std::env` module](https://doc.rust-lang.org/std/env/)
- [`clap` crate documentation](https://docs.rs/clap/)
- [The Rust Book on Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
