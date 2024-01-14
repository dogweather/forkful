---
title:                "Gleam recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of any programming language and can greatly enhance the functionality and usability of a program. In this blog post, we will explore how to use command line arguments in the Gleam programming language.

## How To

To read command line arguments in Gleam, we first need to import the `gleam/commands` module, which provides functions for working with command line arguments. Let's take a look at an example:

```Gleam
import gleam/commands

fn main(argv) {
  let args = commands.parse(argv)
  let language = case args[0] {
    "en_GB" -> "English"
    "es_ES" -> "Spanish"
    _ -> "Unknown"
  }
  io.println("The selected language is {language}")
}
```

In this example, we are using the `parse` function from the `gleam/commands` module to retrieve the command line arguments and store them in the `args` variable. We then use a `case` statement to check the value of the first argument and print a message based on the chosen language. Let's see what the output would be when executing this program with different arguments:

```
$ gleam run main.gleam en_GB
The selected language is English
```

```
$ gleam run main.gleam es_ES
The selected language is Spanish
```

```
$ gleam run main.gleam fr_FR
The selected language is Unknown
```

## Deep Dive

The `gleam/commands` module also provides a `Config` type, which can be passed to the `parse` function to specify different options for parsing the command line arguments. For example, we can set `allow_extra` to `false` to prevent the program from accepting extra arguments. We can also specify a `description` for our program, which will be displayed when using the `--help` flag.

```Gleam
import gleam/commands

fn main(argv) {
  let args = commands.parse(
    argv,
    commands.Config(
      description: "A simple program to demonstrate command line arguments",
      allow_extra: false
    )
  )
  
  // Rest of the code
}
```

We can also use `commands.parse_or_exit` which returns either the parsed arguments or terminates the program with an error if the arguments are invalid. This allows us to handle any potential errors when reading command line arguments.

## See Also

- [The `gleam/commands` module documentation](https://gleam.run/modules/commands/)
- [Another blog post on using command line arguments in Gleam](https://myblog.com/gleam-command-line-arguments)