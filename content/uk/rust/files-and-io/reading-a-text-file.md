---
date: 2024-01-20 17:55:09.110956-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Old days had it rough; reading files was low-level and cumbersome. Rust\
  \ simplifies it, safely. Alternatives exist:\u2026"
lastmod: '2024-04-05T22:51:02.088391-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Old days had it rough; reading files was low-level and cumbersome."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## How to: (Як це зробити:)
```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("hello.txt")?; // Open a file
    let mut contents = String::new();
    file.read_to_string(&mut contents)?; // Read file contents
    println!("File Contents:\n{}", contents);
    Ok(())
}
```
Output:
```
File Contents:
Hello, Ukraine!
```

## Deep Dive (Поглиблений розбір)
Old days had it rough; reading files was low-level and cumbersome. Rust simplifies it, safely. Alternatives exist: `read_to_string` for all-at-once, `BufReader` for efficiency with large files. Under the hood, Rust leans on OS abstractions, ensuring safety and speed.

## See Also (Дивись також):
- The Rust Book on file I/O: https://doc.rust-lang.org/book/ch12-02-reading-from-a-file.html
- Rust by Example for practical scenarios: https://doc.rust-lang.org/stable/rust-by-example/std_misc/file.html
- Error handling in Rust: https://blog.burntsushi.net/rust-error-handling
