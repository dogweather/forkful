---
title:                "Reading command line arguments"
html_title:           "Gleam recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is all about obtaining user inputs passed via the terminal when launching the program. Programmers do it to customize the behavior of their programs - pretty cool.

## How to:

Fire up your terminal. Below is a simple program to read the command-line arguments using Gleam.

```Gleam
import gleam/io.{println}
import gleam/option.{to_result}
import gleam/result.{Ok}

fn start(args: List(String)) -> Result(Never, Nil) {
  args
  |> list.pop
  |> option.to_result(Nil)
  |> result.map(println)
  |> result.unwrap(Never)
}
```
To test it, compile the program and pass some argument like “Hello, Gleam!”:

```command
$ ./my_program "Hello, Gleam!"
Hello, Gleam!
```
## Deep Dive

Historically, command-line arguments exist since the early Unix days, when GUIs were more a dream than reality. Loved by shell scripters worldwide, they significantly enhance a program's flexibility.

In Gleam, you may notice the absence of a specific function to read command-line arguments as in other languages. Instead, `start/1` gets a list of arguments, the first being the script name. It's part of Gleam's philosophy to keep core language lightweight.

Also, there's the `gleam/option` and `gleam/result` modules. `option.to_result/2` converts an option into a result and `result.map/2` then prints the argument, while `result.unwrap/1` crashes the program in the Nil case. Gleam’s take on error handling, indeed. 

## See Also: 

- The Gleam Book: [https://gleam.run/book/](https://gleam.run/book/)
- Gleam GitHub Repository: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Erlang -- init:get_argument/1: [https://erlang.org/doc/man/init.html#get_argument-1](https://erlang.org/doc/man/init.html#get_argument-1)

Dive in. Get your hands dirty with Gleam. It's fun and pretty darn powerful. Happy coding, folks.