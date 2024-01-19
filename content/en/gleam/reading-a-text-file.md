---
title:                "Reading a text file"
html_title:           "Gleam recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is the process of using code to open and retrieve the contents of a file in a human-readable format. Programmers need to do this to access and manipulate data or configuration values that are stored in these files.

## How to:

Below is a basic demonstration of how you might read and print the contents of a text file in Gleam.

```Gleam
import gleam/oki.@{read_file}

pub fn main(_) {
  case read_file("filename.txt") {
    Ok(contents) -> 
      let _ = io.println(contents)
    Error(err) -> 
      let _ = io.println(err)
  }
}
```

```Output
Hello, Gleam!
```
In this example, the code reads the content of a file named "filename.txt" and then prints its content to the console. If the file can't be found, or there's an error while reading it, the program prints the error description.

## Deep Dive

Reading text files is a feature that has its roots in the earliest days of programming, dating back long before the rise of modern high-level languages. The need to interact with files stored on disk is nearly as old as computers themselves, and various methods have been implemented over the years to facilitate this.

In the Gleam language environment, the primary way to interact with text files is through the `gleam/oki` library's `read_file/1` function, as demonstrated above. The function itself implements basic error handling, but depending on the particular use case, more specific error handling might be desired for more robust applications.

In terms of alternatives, there's actually another built-in option for reading files in a more incremental, chunk-based manner, namely `oki.read_file_stream/1`. This function is typically preferred when working with very large files that might not fit into memory all at once.

In the implementation, the file reading functions use Erlang's file handling mechanisms under the hood. The file content is returned as a binary, and it's converted (if necessary) to the list of integers representing Unicode code points for processing in Gleam.

## See Also

1. [Gleam: A statically typed language for the Erlang ecosystem](https://gleam.run/)
2. [Gleam/oki library: Basic File and I/O Operations](https://hexdocs.pm/gleam_stdlib/gleam/oki/)
3. [Erlang: File Module](http://erlang.org/doc/man/file.html)