---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is a crucial task in programming where you verify whether a specific file path points to an existing directory. Programmers do this to avoid errors caused by non-existent directories and to establish that a location is ready to be accessed or manipulated.

## How to:
Let's walk through a simple way to do this in Gleam. Currently, Gleam doesn't have built-in support for file system operations, but you can use Erlang's `:filelib` module to do this:

```Gleam
let directory_exists = :filelib.is_dir("your_directory_path")
```

Next, you can handle as per the `directory_exists` value. For instance:

```Gleam
case directory_exists {
  True -> "Directory exists"
  False -> "Directory does not exist"
}
```

The output will be either "Directory exists" or "Directory does not exist", based on the actual scenario.

## Deep Dive
Historically, Gleam, like many functional languages, has relied heavily on the underlying language or runtime for I/O operations. In Gleam's case, that's the Erlang BEAM virtual machine.

There are a few alternatives for checking if a directory exists. For instance, if you are open to direct system commands, you can use `os.cmd`:

```Gleam
let output = os.cmd("test -d your_directory_path && echo Exists || echo Does not exist")
```

However, remember that system commands heavily rely on the underpinning OS and can be a potential security risk.

In the context of the `:filelib.is_dir` function, the actual implementation exists within Erlang. It's a blocking function, meaning that it'll block the process until it retrieves the result, essentially making it synchronous.

## See Also
For more insights on Erlang's `:filelib` module, visit http://erlang.org/doc/man/filelib.html

Gleam program construction: https://gleam.run/book/tour/modules.html 

Erlang's handling of system commands: http://erlang.org/doc/man/os.html#cmd-1