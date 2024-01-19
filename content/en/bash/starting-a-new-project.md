---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting a New Project in Bash

## What & Why?

Let's clear this up right away. Starting a new project in Bash implies assembling a set of structured scripts to accomplish a task or software. We do it to organize our work, make it reusable and manageable.

## How to:

Easy peasy! Start a new script with a text editor, and use the shebang (#!) to specify the shell interpreter.

```Bash
#!/bin/bash
echo "Hello, world!"
```

Just run the script to see the output.

```Bash
$ ./myscript.sh
Hello, world!
```

Remember, to run it, you must add execution permission:

```Bash
$ chmod +x myscript.sh
```

## Deep Dive

Historically, Bash has been the default shell in various Unix and Linux distributions. It was released in 1989 as a replacement for the then-default Bourne shell, hence the name, Bash (Bourne Again SHell).

Dealing with alternatives, 'sh' is the original Unix shell. However, Bash is more feature-loaded. You also have Zsh and Fish, in the race, offering more user-friendly and interactive features.

About implementation, here's where it gets a tad bit tricky. The `.bashrc` file is used to configure your Bash sessions, and you can call your scripts from it. This is a potent feature, but be careful - any errors in this file can lead to a non-functioning terminal session!

## See Also

For more on Bash scripting, check these out:

- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash Beginners Guide](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [ShellCheck](https://www.shellcheck.net/), a tool for linting your shell scripts.