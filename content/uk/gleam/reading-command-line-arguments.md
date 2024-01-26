---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:55:56.754439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
## Що та Чому?

Reading command line arguments lets programs act on different inputs without changing code. We do it to make programs flexible and reusable.

## How to:
## Як це зробити:

```gleam
import gleam/io
import gleam/os

pub fn main() {
  let args = os.args()
  match args {
    [] -> io.println("No arguments provided.")
    [first, ..] -> io.println("First argument: " ++ first)
  }
}
```

Sample Output:
```
First argument: example.txt
```

## Deep Dive
## Поглиблений Розгляд

Gleam, a static-typed language on the BEAM, grabs command line arguments using the `os.args()` function, inherited from Erlang's powerful capabilities. Historically, command line parsing is from Unix's early days; it's common across languages. Alternatives like argument-parsing libraries exist, but `os.args()` is fine for basics. It gives a list; you handle logic and errors.

## See Also:
## Додатково:

- Gleam Book on Command Line Applications: https://gleam.run/book/tour/command-line-applications.html
- Erlang's `init:get_args/1` documentation for broader understanding: http://erlang.org/doc/man/init.html
- Rust language's 'Clap' crate for comparison with an argument parsing library: https://crates.io/crates/clap
