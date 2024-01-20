---
title:                "Writing a text file"
html_title:           "Gleam recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file is a process of creating and saving data in a file with a .txt extension. Programmers do it for various purposes including log generation, data recording, or for providing input data to programs and scripts.

## How to:

Now, let's check out how to write a text file in Gleam. Note that Gleam doesn't yet have a native file I/O, so we have to interface with Erlang functions.

Ensure to have the latest Gleam version. Now, let's create a Gleam file "file_io.gleam":

```Gleam
import erlang

fn main(args: List(String)) {
  write_file(args)
}

fn write_file(args) {
  case args {
    [.. _, content, filename] -> erlang:display(erlang:file:write_file(filename, content))
    _ -> erlang:display("Incorrect Arguments")
  }
}
```
In the shell, compile and generate a binary:

```shell
rebar3 eunit
_build/default/bin/file_io "Hello, world!" output.txt
```

## Deep Dive

Gleam, an ambitious statically-typed language built for BEAM, the Erlang virtual machine, presently doesn't have in-built support for file I/O. Instead, it employs Erlang's comprehensive I/O library.

In the above example, we used functions `file:write_file/2` from the Erlang module. It takes a filename and the content that you want to write, creating the file if it doesn't exist, and writes the content in it.

An alternative to write file could be employing Elixir functions from within Gleam as they share the same BEAM.

## See Also

2. [Erlang/OTP documentation](http://erlang.org/doc/apps/stdlib/index.html)
3. [Elixir official documentation](https://elixir-lang.org/docs.html)
5. [Erlang File Module documentation](https://erlang.org/doc/man/file.html)