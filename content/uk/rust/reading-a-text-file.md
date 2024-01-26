---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:55:09.110956-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Reading a text file means pulling text data from a file on your system - it's basic I/O. Programmers need this to access and manipulate data, like configs or logs.

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
