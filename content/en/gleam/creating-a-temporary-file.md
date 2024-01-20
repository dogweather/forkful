---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file means generating a file that's meant to exist only until a particular program finishes. Programmers do this to temporarily store large data, reduce memory usage or share data between different parts of a program.

## How to:

In Gleam, you can create a temp file using core libraries. Here's how:

```Gleam
import gleam/otp/process
import gleam/erlang/erlang
import gleam/io/filesystem.{Dir, File}

pub fn main() {
  let pid = process.self()
  let temp_dir = erlang.make_ref()
    |> erlang.term_to_binary
    |> erlang.binary_to_list
  let temp_file = File.append_to(Dir.from_string("..") / Dir.from_string(temp_dir), "temp.txt")

  erlang.io(fwrite("~p~n", [pid]))
  erlang.io(fwrite("Temp file created: ~s~n", [File.to_string(temp_file)]))
}
```
When this code is run, it will generate a temporary file `temp.txt` in the directory pointed at by `temp_dir`, which is a uniquely named directory.

## Deep Dive

Historically, temp files were used in early programming to manage memory constraints. As memory sizes increased, this became less critical but still useful for efficiency, particularly in systems dealing with large data sets or processes.

Some alternatives to temp files include: using a database, in-memory data structures like lists or maps, or storing the data in the cloud. However, each approach comes with its own trade-offs.

Gleam leverages the existing Erlang library in its implementation of temp files. This gives excellent interoperability with Erlang and Elixir code, meaning you can confidently script and manage your infrastructure in Gleam.

## See Also

* [Erlang docs on file operations](http://erlang.org/doc/man/file.html)
* [A guide to temp files in Unix](https://www.tldp.org/LDP/abs/html/tempfiles.html)