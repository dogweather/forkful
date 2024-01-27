---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file means saving data as text that humans can read. Programmers do it to store output, configure apps, or log events.

## How to:
Gleam offers file I/O through its standard library. Here's how to write to a file:
```gleam
import gleam/io
import gleam/result

pub fn write_to_file(contents: String) -> Result(Nil, IOError) {
  result.then(
    io.open("output.txt", [io.Write]),
    fn(file) { io.write(file, contents) }
  )
}

pub fn main() {
  case write_to_file("Hello, Gleam!") {
    Ok(_) -> io.print("Successfully wrote to the file")
    Error(err) -> io.print(err)
  }
}
```
If successful, your `output.txt` will contain "Hello, Gleam!".

## Deep Dive
Historically, file handling is critical for long-term data storage. Gleam's approach is similar to Erlang's, upon which it's built. Alternatives include database systems or in-memory storage for temporary data. Gleam's standard library keeps a lean API, preferring explicit error handling with the `Result` type.

## See Also
- [Erlang's File module](http://erlang.org/doc/man/file.html) for understanding the lower-level operations Gleam abstracts over.
- [Writing files in Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.create), for a comparison with another systems language.
