---
title:                "Reading command line arguments"
date:                  2024-01-20T17:55:52.760230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets programs act on data passed during launch. Programmers use it for customizing behavior without altering code.

## How to:

Gleam's `main` function can access command line arguments through a list of strings. Iterate, pattern match, and do as needed.

```gleam
import gleam/io

fn main(args: List(String)) {
  let message = match args {
    [] -> 
      "No arguments found."
    [single] -> 
      single
    _ -> 
      "Too many arguments!"
  }
  io.println(message)
}
```

Running the program:

```
$ my_program
No arguments found.

$ my_program "Hello, Gleam!"
Hello, Gleam!

$ my_program Too many arguments given
Too many arguments!
```

## Deep Dive

Reading command line arguments has been a staple in programming since the early days. UNIX utilities excel here. Gleam, while rooted in the Erlang VM, offers a modern touch to this functionality. Alternatives include parsing libraries for complex cases, like options flags. Gleam does this without the verbosity of Erlang or the obscurity of C.

## See Also

For further exploration:

- Gleam's official documentation: https://gleam.run/book
- Erlang's `escript`: http://erlang.org/doc/man/escript.html
- Command-line parsing libraries: See Gleam's package repository at https://hex.pm/packages?search=gleam