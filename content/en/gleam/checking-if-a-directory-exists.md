---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:56:04.519559-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is just like peeking into a room to see if it's there before you step in. Programmers do it to avoid errors when trying to access or modify directories that might not be present.

## How to:

To check if a directory exists in Gleam, we need to interact with the file system. Unfortunately, as of my knowledge cutoff in 2023, Gleam doesn't have built-in file system operations as it's primarily designed for building fault-tolerant systems. However, we can interface with Erlang's file system operations thanks to Gleam's ability to use Erlang functions.

Here's a quick example using Erlang's `file` module:

```Gleam
import gleam/erlang
import gleam/io

fn does_directory_exist(dir: String) -> Bool {
  case erlang.apply(
    module: "file", 
    function: "read_file_info", 
    args: [dir]
  ) {
    Ok(_) -> true
    Error(_) -> false
  }
}

fn main() {
  let directory = "/some/path/to/directory"
  let exists = does_directory_exist(directory)
  io.println(exists)
}
```

This might output `true` if the directory exists, and `false` if it doesn't.

## Deep Dive

Historically, Gleam is young and evolving. It's built on the BEAM virtual machine, which Erlang uses, and thus inherits Erlang's robust features, including file system operations. Without native support for these operations, we must turn to Erlang interoperability.

Alternatives to checking directory existence depend on the underlying system. In other programming languages, there might be direct calls or standard libraries available for these tasks. For instance, in Python, you'd use `os.path.isdir()`.

Implementation details with the `file.read_file_info` function in Erlang tell us that it returns a tuple that includes the file info if the operation succeeded or an error tuple if it failed. These tuples can then be pattern matched to determine the outcome. The success tuple looks like `{:ok, FileInfo}` while an error is represented as `{:error, Reason}`.

While adopting Erlang libraries in Gleam is currently the way forward for tasks like these, it's worth noting that the community might introduce a dedicated package for file system interactions in the future.

## See Also

- [Erlang 'file' module documentation](http://erlang.org/doc/man/file.html)
- [Erlang error reasons for file operations](http://erlang.org/doc/apps/stdlib/io_protocol.html)
