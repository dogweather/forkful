---
date: 2024-01-25 03:39:28.833576-07:00
description: "A Rust interactive shell, or REPL (Read-Eval-Print Loop), lets you run\
  \ Rust code on-the-fly, seeing instant results, perfect for experimentation or\u2026"
lastmod: '2024-03-13T22:44:59.897288-06:00'
model: gpt-4-1106-preview
summary: "A Rust interactive shell, or REPL (Read-Eval-Print Loop), lets you run Rust\
  \ code on-the-fly, seeing instant results, perfect for experimentation or\u2026"
title: Using an interactive shell (REPL)
---

{{< edit_this_page >}}

## What & Why?
A Rust interactive shell, or REPL (Read-Eval-Print Loop), lets you run Rust code on-the-fly, seeing instant results, perfect for experimentation or learning. Programmers use it to test snippets, debug, or just play with language features without the overhead of compiling a full project.

## How to:
As of now, Rust does not have an official REPL shipped with it. You can use third-party tools like `evcxr_repl`. Install it with Cargo:

```sh
cargo install evcxr_repl
```

Then, run the REPL:

```sh
evcxr
```

Inside, test some Rust code:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Output should be:

```
5 + 3 = 8
```

## Deep Dive
Rust's ethos is centered around safety and performance, which are usually associated with ahead-of-time compiled languages, and less with interpreted, REPL-friendly ones. Historically, languages like Python or Ruby prioritized having a REPL for immediate feedback, but weren't designed with system-level tasks in mind.

Despite the absence of an official REPL in Rust, a couple of alternatives like `evcxr_repl` have emerged. These projects are not just hacking Rust into a REPL; they're smartly weaving together the language's compile-and-run cycle into an interactive session. The REPL compiles the code behind the scenes and runs the binary, capturing the output. This way, it preserves Rust's performance benefits while still giving that interactive experience.

There's ongoing discussion in the Rust community about official REPL support, and with every language iteration, we see more tooling sophistication that might eventually lead to a native solution.

## See Also
For more info and other tools:
- Evcxr REPL GitHub repo: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, an online way to test Rust code: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Rust Language discussion on REPL feature: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
