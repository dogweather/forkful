---
title:                "Checking if a directory exists"
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Checking If A Directory Exists in Gleam: A Practical Guide

## What & Why?

Checking if a directory exists means verifying whether a specific folder is present in a given file system path or not. Programmers often use it to avoid errors when trying to access files in non-existent folders, or when needing to create a new directory only if it does not already exist.

## How to:

In Gleam, there's no built-in function for directory inspections yet. However, you can execute shell commands and use Erlang's `filelib:is_dir/1` function. Here's the code:

```Gleam
import gleam/io.{println}
import gleam/otp/process.{spawn}

pub fn main(args: List(String)) {
    case args {
    | [directory] ->
        let _ = spawn(fn(_) {
            case shell("filelib", "is_dir", [directory]) {
            | result -> 
                case result {
                | True -> println(directory ++ " exists.")
                | False -> println(directory ++ " does not exist.")
                }
            }
        })
    | _ -> 
        println("Provide a directory name as an argument.")
    }
}
```

To execute this, enter the directory as an argument, e.g., `gleam run main ./existing_directory`. The output will be:

```
./existing_directory exists.
```

Now enter a non-existing directory, e.g., `gleam run main ./non_existing_directory`. The output will be:

```
./non_existing_directory does not exist.
```

## Deep Dive

Historically, checking if a directory exists is a common operation in many programming languages and their standard libraries. But Gleam, being a statically typed functional programming language designed for building scalable concurrent systems, is still evolving and doesn't have a built-in function for this task yet.

There are alternative ways to accomplish this. Apart from shell commands, you could potentially use Erlang's `filelib:is_dir/1` function directly. However, that needs interfacing with Erlang code, which may not be desirable for all Gleam developers. 

The implementation is straightforward, simply invoking the command or function and interpreting its result, but key is knowing about this possibility, due to lack of Gleam-specific documentation or native support for such filesystem tasks.

## See Also

To dive deeper into Gleam and its features, visit the official Gleam documentation [here](https://gleam.run/docs/). You can explore the Erlang's `filelib:is_dir/1` function [here](http://erlang.org/doc/man/filelib.html) and learn more about integrating Erlang code with Gleam from [this tutorial](https://gleam.run/news/gleam-v0.14-released/).