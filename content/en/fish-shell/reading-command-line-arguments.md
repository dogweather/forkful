---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the act of acquiring data input at the program's execution command. Programmers do this to allow for dynamic behavior, customization, and control of the program's execution.

## How to:

Let's write a simple Fish function which accepts command line arguments.

```Fish
function hello
    set name $argv[1]
    echo "Hello, $name!"
end
```

Save the above code as `hello.fish`, then execute it:

```Fish
fish hello.fish John
```

You should see:

```
Hello, John!
```

Terrific. The `$argv` variable represents the array of arguments, with `$argv[1]` holding the first argument.

## Deep Dive

Historically, command line arguments have been used since the earliest days of programming to control programs' behaviors. Even today in the interactive and GUI-driven era, they're critically important tools for refining program execution.

Alternatives? Sure, you could use environment variables or configuration files to determine behavior, but they are less spontaneous and immediate than command line arguments.

In Fish, command-line arguments live in the `$argv` variable. Arguments are just raw string data until you decide what to do with them. For example, you might want to treat an argument as a number and perform arithmetic, or as a file name and read its contents. It's quite flexible.

## See Also

- Fish Shell Official Documentation: [Command Line Arguments](https://fishshell.com/docs/current/index.html#syntax)
- Practical Fish: [Using Command Line Arguments](https://practical-fish.readthedocs.io/en/latest/commands.html#command-line-arguments)