---
title:                "Reading a text file"
date:                  2024-01-20T17:54:51.510451-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file means pulling in data from a file composed of text on your disk. Programmers do this to access and manipulate stored information, like configs, logs, or any data their apps need.

## How to:
Gleam doesn't include file IO in its standard library, so we'll use Erlang functions. We start by opening a text file using `file.open/2`, read its content, handle potential errors, and finally close the file. Here's the beef:

```gleam
import gleam/erlang
import gleam/result

fn main() {
  case erlang.file.open("example.txt", [read]) {
    Ok(file) ->
      case erlang.file.read(file) {
        Ok(data) -> {
          erlang.io.format("Content: ~p~n", [data])
          erlang.file.close(file)
        }
        Error(err) -> {
          erlang.io.format("Error reading file: ~p~n", [err])
        }
      }
    Error(err) ->
      erlang.io.format("Error opening file: ~p~n", [err])
  }
}
```

Run this and you'll see your text file's content, or an error if something went sideways.

## Deep Dive
Reading files is nothing new; it's been in programming since the punch card days. Gleam, a static-typed language that compiles to the Erlang VM, leans on Erlang's mature ecosystem for file operations. You've got other options, too: async reads, streaming lines, or using libraries like `gleam_otp` for a more Gleam-ish approach.

Understanding file IO includes error handling. Files might not exist, could be locked, or you might lack permission. Gleam's pattern matching and `result` module give you a clean path for managing the unexpected.

Lastly, consider your file's size. Our simple `erlang.file.read` reads the whole thing into memory, which could be problematic for huge files. Streaming chunks or lines would be more efficient.

## See Also
- [Erlang's file module docs](http://erlang.org/doc/man/file.html) since we're using Erlang's capabilities.
- [Erlang's IO docs](http://erlang.org/doc/apps/stdlib/io_protocol.html) for understanding how input/output works under the hood.